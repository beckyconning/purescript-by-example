-- Generated by psc-make version 0.6.9.3
module Data.Maybe.Unsafe where
import Prim ()
import Prelude ()
import Data.Maybe ()
--  | A partial function that extracts the value from the `Just` data
--  | constructor. Passing `Nothing` to `fromJust` will throw an error at
--  | runtime.
foreign import fromJust :: forall a. Data.Maybe.Maybe a -> a
