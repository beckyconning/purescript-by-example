module ParallelExercises where

import Control.Alt
import Control.Apply
import Control.MonadPlus
import Control.Monad.Cont.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Parallel
import Control.Monad.Error.Trans

import Data.Array (filter, concat)
import Data.Either
import Data.Maybe
import Data.String (joinWith)
import Data.Traversable

import Debug.Trace

import Files

import Network.HTTP.Client

import SetTimeout

getResponseText :: forall eff. Request -> ContT Unit (Eff (ref :: Ref, http :: HTTP | eff)) String
getResponseText req = responseToString <$> getAll req
  where
  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ runChunk <$> chunks

getResponseText2 :: forall eff. Request -> Request -> ContT Unit (Eff (ref :: Ref, http :: HTTP | eff)) String
getResponseText2 req1 req2 = par (<>) (getResponseText req1) (getResponseText req2)

traceResponseText2 :: forall eff. Request -> Request -> Eff (ref :: Ref, http :: HTTP, trace :: Trace | eff) Unit
traceResponseText2 req1 req2 = flip runContT trace $ getResponseText2 req1 req2

race :: forall a eff. ContRef eff a -> ContRef eff a -> ContRef eff a
race ca cb = ContT $ \k -> do
  r <- newRef false

  runContT ca $ \a -> do
    m <- readRef r
    case m of
      false -> do
        writeRef r true
        k a
      true -> return unit

  runContT cb $ \b -> do
    m <- readRef r
    case m of
      false -> do
        writeRef r true
        k b
      true -> return unit

timeout :: forall a eff. Milliseconds -> ContRef (timeout :: Timeout | eff) a -> ContRef (timeout :: Timeout | eff) (Maybe a)
timeout t c = race t' c'
  where
  t' = setTimeoutCont t >>= \_ -> return Nothing
  c' = c >>= \a -> (return $ Just a)

rights :: forall a b. [Either a b] -> [b]
rights = concat <<< (right <$>)
  where
  right = (either (\_ -> []) (\x -> [x]))

readMany :: forall eff. [FilePath] -> Parallel (fs :: FS | eff) [String]
readMany filePaths = rights <$> readFilePar `traverse` filePaths
  where
  readFilePar filePath = Parallel $ readFileCont filePath

instance altParallel :: Alt (Parallel eff) where
  (<|>) (Parallel x) (Parallel y) = Parallel $ race x y

-- We can't write Alternative instance for Parallel because we can't have a Plus instance for
-- ContT. This is because there is no reasonable definition of empty for ContT.

parallelExample = lift2 (++) (ErrorT (Parallel (readFileCont "/tmp/1.txt"))) (ErrorT (Parallel (readFileCont "/tmp/2.txt"))) 

readManyErr :: forall eff. [FilePath] -> ErrorT ErrorCode (Parallel (fs :: FS | eff)) [String]
readManyErr = traverse readFilePar
  where
  readFilePar filePath = ErrorT $ Parallel $ readFileCont filePath
