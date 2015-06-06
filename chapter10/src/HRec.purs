module HRec where

import Data.Function
import Data.Maybe

foreign import data HRec :: * -> *

--instance showHRec :: (Show a) => Show (HRec a) where
--  show rec = "HRec { " ++ Data.String.joinWith ", " (runFn3 foldHRec f [] rec) ++ " }"
--    where
--    f label value = show k ++ ": " ++ show a
--    values =
--    labels = runFn2 mapHRecLabel (\label _ -> label + ": ") rec

foreign import empty
  "var empty = {}" :: forall a. HRec a

foreign import insert
  "function insert(rec, key, value) {\
  \  var copy = {};\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      copy[k] = rec[k];\
  \    }\
  \  }\
  \  copy[key] = value;\
  \  return copy;\
  \}" :: forall a. Fn3 (HRec a) String a (HRec a)

foreign import mapHRec
  "function mapHRec(f, rec) {\
  \  var mapped = {};\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      mapped[k] = f(rec[k]);\
  \    }\
  \  }\
  \  return mapped;\
  \}" :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)

foreign import mapHRecLabel
  "function mapHRec(f, rec) {\
  \  var mapped = {};\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      mapped[k] = f(k, rec[k]);\
  \    }\
  \  }\
  \  return mapped;\
  \}" :: forall a b. Fn2 (String -> a -> b) (HRec a) (HRec b)

instance functorHRec :: Functor HRec where
  (<$>) f rec = runFn2 mapHRec f rec

foreign import foldHRec
  "function foldHRec(f, r, rec) {\
  \  var acc = r;\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      acc = f(acc, k, rec[k]);\
  \    }\
  \  }\
  \  return acc;\
  \}" :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r

union :: forall a. HRec a -> HRec a -> HRec a
union = runFn3 foldHRec insert

foldHRec' :: forall a r. (r -> String -> a -> r) -> r -> HRec a -> r
foldHRec' f = runFn3 foldHRec (mkFn3 f)

lookup' :: forall a. String -> HRec a -> Maybe a
lookup' labelQuery = foldHRec' f Nothing
  where
  f acc  _    _     | isJust acc          = acc
  f acc label value | label == labelQuery = Just value
  f _    _    _                           = Nothing

foreign import lookupHelper
  "function lookupHelper(nothing, just, key, rec) {\
  \  var value = rec[key];\
  \  return value !== undefined ? just(value) : nothing();\
  \}" :: forall a r. Fn4 r (a -> r) String (HRec a) r

lookup :: forall a. String -> HRec a -> Maybe a
lookup = runFn4 lookupHelper Nothing Just
