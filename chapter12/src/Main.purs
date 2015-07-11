module Main where

import Data.Array (map)
import Data.String (joinWith, length)
import Data.Function
import Data.Either
import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Error.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras
import Control.Monad.Parallel (runParallel)
import Control.Monad.Trans

import Files
import SetTimeout
import Spider
import ParallelExercises

import Network.HTTP.Client

import Debug.Trace

--main = runContT (getResponseText purescript_org) trace
--  where
--  getResponseText req = responseToString <$> getAll req
--
--  responseToString :: Response -> String
--  responseToString (Response chunks) = joinWith "" $ map runChunk chunks
--
--  purescript_org :: Request
--  purescript_org = Request
--    { host: "www.purescript.org"
--    , path: "/"
--    }

-- main = concatFile "/Users/beckyconning/1.txt" "/Users/beckyconning/2.txt" "/Users/beckyconning/3.txt"

-- main = runContT example (\_ -> return unit)

-- main = runContT (getCont (Request { host: "www.google.com", path: "/" })) (trace <<< show)

--main = runContT (getResponseText request) $ \response -> do
--  let responseLength = length response
--  trace $ show responseLength
--
--  where
--  request :: Request
--  request = Request
--    { host: "www.purescript.org"
--    , path: "/"
--    }

--main = runContT (f $ getCont req) (trace <<< show)
--  where
--  f :: forall eff. ContRef eff (Maybe Chunk) -> ContRef eff Number
--  f = foldC g 0
--
--  req :: Request
--  req = Request { host: "www.purescript.org", path: "/" }
--
--  g :: Number -> Maybe Chunk -> Either Number Number
--  g x Nothing = Right x
--  g x (Just (Chunk y)) = Left $ x + (length y)

--main = runContT
--  (race
--    (setTimeoutCont 2000 >>= \_ -> return "hello")
--    (setTimeoutCont 5000 >>= \_ -> return "goodbye"))
--  trace

main = runContT (runErrorT $ getAllReferences [exampleFilePath]) print
