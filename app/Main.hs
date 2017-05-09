{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Query profiling files and generate reports.

module Main where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CL (dropWhile)
import qualified Data.Conduit.List as CL
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.IO as LT
import           Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import           GHC.Prof
import qualified GHC.Prof.Parser as GHCProf
import           Options.Applicative.Simple
import           System.Environment
import           Text.Printf

--------------------------------------------------------------------------------
-- Types

data CostCentres = CostCentres
  { costCentresTime :: !Scientific
  , costCentresAlloc :: !Scientific
  } deriving (Show)

data SortBy
  = Time
  | Alloc
  deriving (Show)

data Order
  = Asc
  | Desc
  deriving (Show)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  (m, ()) <-
    simpleOptions
      "0"
      "profile-query"
      "Query .prof files"
      ((\file name limit cmp order ->
          maybe (general file limit cmp order) (specific file) name) <$>
       strOption (long "file" <> short 'f') <*>
       optional (strOption (long "name" <> short 'n')) <*>
       option auto (long "limit" <> short 'l' <> value 30) <*>
       option
         (maybeReader
            (\case
               "time" -> Just Time
               "alloc" -> Just Alloc
               _ -> Nothing))
         (long "sort" <> short 's' <> value Time) <*>
       option
         (maybeReader
            (\case
               "asc" -> Just Asc
               "desc" -> Just Desc
               _ -> Nothing))
         (long "order" <> value Desc))
      empty
  m

-- | General summary for all cost centres.
general :: FilePath -> Int -> SortBy -> Order -> IO ()
general fp limit cmp order = do
  !histogram <- runConduitRes (CB.sourceFile fp .| CB.lines .| generalHistogram)
  mv <- V.unsafeThaw (V.fromList (M.toList histogram))
  let comparison = ordering (field . snd)
        where
          field =
            case cmp of
              Time -> costCentresTime
              Alloc -> costCentresAlloc
          ordering =
            case order of
              Asc -> comparing
              Desc -> flip . comparing
  V.sortBy comparison mv
  v <- V.freeze mv
  putStrLn
    (tablize
       ([[(True, "Name"), (False, "%alloc"), (False, "%time")]] ++
        map
          (\(name, cost) ->
             [ (True, T.unpack name)
             , (False, formatScientific Fixed (Just 2) (costCentresAlloc cost))
             , (False, formatScientific Fixed (Just 2) (costCentresTime cost))
             ])
          (V.toList (V.take limit v))))

-- | Summarize for a specific cost centre name.
specific :: FilePath -> String -> IO ()
specific fp sym = do
  total <-
    runConduitRes
      (CB.sourceFile fp .| CB.lines .| specificSum sym .|
       CL.fold
         (\CostCentres { costCentresTime = (!totalTime)
                       , costCentresAlloc = (!totalAlloc)
                       } cost ->
            CostCentres
            { costCentresTime = costCentreInhTime cost + totalTime
            , costCentresAlloc = costCentreInhAlloc cost + totalAlloc
            })
         (CostCentres 0 0))
  putStrLn
    ("Total %alloc: " ++
     formatScientific Fixed (Just 2) (costCentresAlloc total))
  putStrLn
    ("Total %time: " ++
     formatScientific Fixed (Just 2) (costCentresTime total))

--------------------------------------------------------------------------------
-- A conduit for querying specific cost centres

specificSum
  :: MonadThrow m
  => String -> Conduit ByteString m CostCentre
specificSum sym = profileCostCentres (CL.concatMapAccum collect Nothing)
  where
    prefix = S8.pack (sym ++ " ")
    collect line mcolumn =
      case mcolumn of
        Nothing ->
          if isPrefix
            then (Just indentation, [line])
            else (Nothing, [])
        Just column ->
          if indentation <= column
            then (if isPrefix
                    then (Just indentation, [line])
                    else (Nothing, []))
            else (Just column, [])
      where
        indentation = S.length (S.takeWhile (== 32) line)
        isPrefix = S.isPrefixOf prefix (S.dropWhile (== 32) line)

--------------------------------------------------------------------------------
-- A consumer of all cost centres producing a histogram

generalHistogram
  :: (MonadIO m, MonadThrow m)
  => Consumer ByteString m (HashMap Text CostCentres)
generalHistogram =
  profileCostCentres (CL.concatMapAccum collect []) .| CL.fold aggregate mempty
  where
    collect line stack0 =
      ([key | child] ++ stack, [line | not (elem key stack)])
      where
        child = spaces >= length stack
        stack = reverse (take spaces (reverse stack0))
        spaces = S.length (S.takeWhile (== 32) line)
        key = S.takeWhile (/= 32) (S.dropWhile (== 32) line)
    aggregate histogram cost =
      M.insertWith
        (\x y ->
           CostCentres
           { costCentresTime = on (+) costCentresTime x y
           , costCentresAlloc = on (+) costCentresAlloc x y
           })
        (costCentreName cost)
        (CostCentres
         { costCentresTime = costCentreInhTime cost
         , costCentresAlloc = costCentreInhAlloc cost
         })
        histogram

--------------------------------------------------------------------------------
-- A conduit for parsing cost centres

data ProfileException = EndOfInput | ParseError ByteString String
 deriving (Show, Typeable)
instance Exception ProfileException

-- | Given an input stream of lines, wait for the header, and then
-- parse a stream of cost centres.
profileCostCentres
  :: (MonadThrow m)
  => Conduit ByteString m ByteString -> Conduit ByteString m CostCentre
profileCostCentres inner = do
  timestamp
  newline
  exec
  newline
  totalTime
  totalAlloc
  newline
  _ <- heading
  newline
  topCostCenters
  newline
  h <- heading
  newline
  inner .| CL.mapM (atto (GHCProf.costCentre h))
  where
    newline =
      await >>= maybe (throwM EndOfInput) (const (return ()))
    timestamp = ignore
    exec = ignore
    totalTime = ignore
    totalAlloc = ignore
    topCostCenters = CL.dropWhile (not . S.null) *> newline
    ignore = await *> return ()
    heading = do
      l <-
        do pre <- line
           if S8.unwords (S8.words pre) == "individual inherited"
             then line
             else pure pre
      atto GHCProf.header l
    line = await >>= maybe (throwM EndOfInput) pure
    atto p bytes = do
      case A.parseOnly p (T.decodeUtf8 (S.dropWhile (== 32) bytes)) of
        Left e -> throwM (ParseError bytes e)
        Right !v -> do
          pure v

--------------------------------------------------------------------------------
-- Handy UI facilities

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',(left',text')) = printf ("%" ++ direction ++ show width ++ "s") text'
          where direction = if left'
                               then "-"
                               else ""
                width = maximum (map (length . snd . (!! x')) xs)
