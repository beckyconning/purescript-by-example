module Spider where

import Control.Apply
import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans
import Control.Monad.Parallel
import Data.Array (concat)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Traversable
import Files

exampleFilePath :: FilePath
exampleFilePath = "/Users/beckyconning/Documents/purescript-book/chapter12/spider/1.json"

-- Throws errors from reading files but ignores those from parsing reference files
getAllReferences :: forall eff. [FilePath] -> ErrorT ErrorCode (ContRef (fs :: FS | eff)) [FilePath]
getAllReferences filePaths = (<>) <$> references <*> nextReferences
  where
  referencesPar :: ErrorT ErrorCode (Parallel (fs :: FS | eff)) [FilePath]
  referencesPar = concat <$> getDirectReferences `traverse` filePaths

  references :: ErrorT ErrorCode (ContRef (fs :: FS | eff)) [FilePath]
  references = mapErrorT runParallel referencesPar

  nextReferences :: ErrorT ErrorCode (ContRef (fs :: FS | eff)) [FilePath]
  nextReferences = references >>= getAllReferences

  getDirectReferences :: forall eff. FilePath -> ErrorT ErrorCode (Parallel (fs :: FS | eff)) [FilePath]
  getDirectReferences filePath = runFReferences <$> (readReferencesJSON <$> readFileParErr filePath)
    where
    runFReferences :: F [FilePath] -> [FilePath]
    runFReferences = either (\err -> [show err]) id

    readFileParErr :: forall eff. FilePath -> ErrorT ErrorCode (Parallel (fs :: FS | eff)) String
    readFileParErr = ErrorT <<< Parallel <<< readFileCont

    readReferencesJSON :: String -> F [FilePath]
    readReferencesJSON json = parseJSON json >>= readProp "references" <<< toForeign
