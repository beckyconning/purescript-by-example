module TestParens where

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class
import Data.String (split)
import Data.Maybe

testParens :: String -> Boolean
testParens = k <<< j <<< (g <$>) <<< split ""
  where
  f :: [Number] -> State (Maybe Number) Unit
  f = traverse_ $ \x -> modify (maybe Nothing (h <<< (x +)))

  g :: String -> Number
  g "(" = 1
  g ")" = (-1)
  g _   = 0

  h :: Number -> Maybe Number
  h x | x < 0 = Nothing
  h x         = Just x

  j :: [Number] -> Maybe Number
  j x = execState (f x) (Just 0)

  k :: Maybe Number -> Boolean
  k (Just 0) = true
  k _        = false
