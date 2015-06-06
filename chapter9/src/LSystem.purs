module Main where

import Data.Array (concatMap)

import Control.Monad (foldM)
import Control.Monad.Eff

--import Graphics.Canvas hiding (translate)
import Debug.Trace

lsystem :: forall a m s. (Monad m) =>
                         [a] ->
                         (a -> [a]) ->
                         (s -> a -> m s) ->
                         Number ->
                         s -> m s
lsystem initialSentence produce interpret numberOfProductions state = interpretSentence $ produceSentence initialSentence numberOfProductions
  where
  interpretSentence sentence = foldM interpret state sentence
  produceSentence sentence 0 = sentence
  produceSentence sentence n = produceSentence (concatMap produce sentence) (n - 1)

type Angle = Number
data Alphabet = L | R | F Boolean

type Sentence = [Alphabet]

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

anAngle = Math.pi / 3

initial :: Sentence
initial = [F false]

productions :: Alphabet -> Sentence
productions L = [L]
productions R = [R]
productions (F true) = [F true,L,F false,L,F true,R,F false,R,F true,R,F false,R,F true,L,F false,L,F true]
productions (F false) = [F false,R,F true,R,F false,L,F true,L,F false,L,F true,L,F false,R,F true,R,F false]

interpret :: State -> Alphabet -> Eff (trace :: Trace) State
interpret state L = trace "L"
interpret state R = trace "R"
interpret state (F true) = trace "(F true)"
interpret state (F false) = trace "(F false)"

initialState :: State
initialState = { x: 120, y: 200, theta: 0 }

main = lsystem initial productions interpret 4 initialState
