module Network.HTTP.Client where

import Debug.Trace

import Data.Maybe
import Data.Either
import Data.Function
import Data.String (joinWith, length)

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras

import Control.Monad.Error.Trans

import Files

foreign import data HTTP :: !

newtype Request = Request
  { host :: String
  , path :: String
  }

newtype Chunk = Chunk String

instance showChunk :: Show Chunk where
  show (Chunk s) = "Chunk " ++ show s

newtype Response = Response [Chunk]

instance showResponse :: Show Response where
  show (Response cs) = "Response " ++ show cs

runChunk :: Chunk -> String
runChunk (Chunk s) = s

type WithHTTP eff = Eff (http :: HTTP | eff)

foreign import getImplErr
  "function getImpl(opts, more, done, error) {\
  \  return function() {\
  \    require('http').request(opts, function(res) {\
  \      res.setEncoding('utf8');\
  \      res.on('data', function (s) {\
  \        more(s)();\
  \      });\
  \      res.on('end', function () {\
  \        done();\
  \      });\
  \      res.on('error', function (e) {\
  \        error(e.message);\
  \      };\
  \    }).end();\
  \  };\
  \}" :: forall eff. Fn4 Request
                         (Chunk -> WithHTTP eff Unit)
                         (String -> WithHTTP eff Unit)
                         (WithHTTP eff Unit)
                         (WithHTTP eff Unit)

foreign import getImpl
  "function getImpl(opts, more, done) {\
  \  return function() {\
  \    require('http').request(opts, function(res) {\
  \      res.setEncoding('utf8');\
  \      res.on('data', function (s) {\
  \        more(s)();\
  \      });\
  \      res.on('end', function () {\
  \        done();\
  \      });\
  \    }).end();\
  \  };\
  \}" :: forall eff. Fn3 Request
                         (Chunk -> WithHTTP eff Unit)
                         (WithHTTP eff Unit)
                         (WithHTTP eff Unit)

getChunk :: forall eff. Request ->
                        (Maybe Chunk -> WithHTTP eff Unit) ->
                        WithHTTP eff Unit
getChunk req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getChunkErr :: forall eff. Request ->
                        (Either String (Maybe Chunk) -> WithHTTP eff Unit) ->
                        WithHTTP eff Unit
getChunkErr req k = runFn4 getImplErr req (k <<< Right <<< Just) (k <<< Left) (k $ Right Nothing)

getCont :: forall eff. Request -> ContT Unit (WithHTTP eff) (Maybe Chunk)
getCont req = ContT $ getChunk req

getContErr :: forall eff. Request -> ErrorT String (ContT Unit (WithHTTP eff)) (Maybe Chunk)
getContErr req = ErrorT $ ContT $ getChunkErr req

getAll :: forall eff. Request -> ContT Unit (WithHTTP (ref :: Ref | eff)) Response
getAll req = Response <$> collect (getCont req)

writeResToFile :: forall eff. Request -> FilePath -> C (http :: HTTP, ref :: Ref | eff) (Either ErrorCode Unit)
writeResToFile req path = responseToString <$> getAll req >>= writeFileCont path
  where
  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ runChunk <$> chunks
