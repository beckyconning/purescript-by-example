module Spider where

import Control.Apply
import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans
import Control.Monad.Parallel
import Control.Monad.Eff.Ref
import Data.Array (concat)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Traversable
import Files

exampleFilePath :: FilePath
exampleFilePath = "/Users/beckyconning/Documents/purescript-book/chapter12/spider/1.json"

getAllReferences :: forall eff. FilePath -> EC (ref :: Ref | eff) [FilePath]
getAllReferences filePath = (filePath :) <$> getDirectReferences filePath >>= getManyReferences
  where
  getManyReferences references = concat <$> errorTParallelFor getAllReferences references
  errorTParallelFor f = mapErrorT runParallel <<< traverse (mapErrorT Parallel <<< f)

getDirectReferences :: forall eff. FilePath -> EC eff [FilePath]
getDirectReferences filePath = runFReferences <$> (readReferencesJSON <$> readFileContErr filePath)
  where
  runFReferences :: F [FilePath] -> [FilePath]
  runFReferences = either (\err -> [show err]) id

  readReferencesFileErr :: forall eff. FilePath -> ErrorT ErrorCode (ContRef (fs :: FS | eff)) String
  readReferencesFileErr = readFileContErr

  readReferencesJSON :: String -> F [FilePath]
  readReferencesJSON json = parseJSON json >>= readProp "references" <<< toForeign
