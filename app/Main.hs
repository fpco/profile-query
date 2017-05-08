{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Query profiling files and generate reports.

module Main where

import           Control.Exception
import           Control.Monad.Catch
import           Data.Attoparsec.Text as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CL (dropWhile)
import qualified Data.Conduit.List as CL
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable
import qualified GHC.Prof.Parser as GHCProf
import           GHC.Prof.Types
import           Options.Applicative.Simple

--------------------------------------------------------------------------------
-- Types

data CostCentres = CostCentres
  { costCentresTime :: !Scientific
  , costCentresAlloc :: !Scientific
  } deriving (Show)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  (m, ()) <-
    simpleOptions
      "0"
      "profile-query"
      "Query .prof files"
      ((\file -> maybe (general file) (specific file)) <$>
       strOption (long "file" <> short 'f') <*>
       optional (strOption (long "name" <> short 'n')))
      empty
  m

-- | General summary for all cost centres.
general :: FilePath -> IO ()
general fp = do
  allThings <- runConduitRes (CB.sourceFile fp .| CB.lines .| aggregate)
  return ()

-- | Summarize for a specific cost centre name.
specific :: FilePath -> String -> IO ()
specific fp sym = do
  total <-
    runConduitRes
      (CB.sourceFile fp .| CB.lines .| filterMatching sym .|
       CL.fold
         (\CostCentres { costCentresTime = totalTime
                       , costCentresAlloc = totalAlloc
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

filterMatching
  :: MonadThrow m
  => String -> Conduit ByteString m CostCentre
filterMatching sym = profileCostCentres (CL.concatMapAccum collect Nothing)
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

aggregate
  :: MonadThrow m
  => Consumer ByteString m (Map Text CostCentres)
aggregate =
  profileCostCentres (CL.map id) .| CL.fold const mempty

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
      case parseOnly p (T.decodeUtf8 (S.dropWhile (== 32) bytes)) of
        Left e -> throwM (ParseError bytes e)
        Right !v -> do
          pure v
