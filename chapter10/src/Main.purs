module Main where

import Data.Maybe
import Data.Array (length)
import Data.Either
import Data.Validation
import Data.Foreign
import Data.Foreign.Null
import Data.Foreign.Class
import Data.JSON
import Data.Traversable
import Data.AddressBook
import Data.AddressBook.UI

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Alert
import Control.Monad.Eff.Storage

import Debug.Trace

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String

  , street     :: String
  , city       :: String
  , state      :: String

  , homePhone  :: String
  , cellPhone  :: String
  }

newtype Tagged a b = Tagged (Either a b)

--instance taggedIsForeign :: IsForeign (Tagged a b) where
--  read value = f <$> readProp "tag" value <*> readProp "value" value

instance taggedIsForeign :: (IsForeign a, IsForeign b) => IsForeign (Tagged a b) where
read object = readProp "tag" object >>= \tag -> case tag of
  "Left" -> readProp "value" object >>= \value -> return $ Tagged $ Left value
  "Right" -> readProp "value" object >>= \value -> return $ Tagged $ Right value

instance formDataIsForeign :: IsForeign FormData where
  read value = mkFormData
    <$> readProp "firstName" value
    <*> readProp "lastName"  value
    <*> readProp "street"    value
    <*> readProp "city"      value
    <*> readProp "state"     value
    <*> readProp "homePhone" value
    <*> readProp "cellPhone" value
    where
    mkFormData :: String -> String -> String -> String -> String -> String -> String -> FormData
    mkFormData firstName lastName street city state homePhone cellPhone = FormData
      { firstName  : firstName
      , lastName   : lastName
      , street     : street
      , city       : city
      , state      : state
      , homePhone  : homePhone
      , cellPhone  : cellPhone
      }

toFormData :: Person -> FormData
toFormData (Person p@{ address = Address a
                     , phones = [ PhoneNumber pn1
                                , PhoneNumber pn2
                                ]
                     }) =
  FormData { firstName  : p.firstName
           , lastName   : p.lastName

           , street     : a.street
           , city       : a.city
           , state      : a.state

           , homePhone  : pn1.number
           , cellPhone  : pn2.number
           }


updateForm :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
updateForm sel value = do
  Just element <- querySelector sel
  setValue value element
  return unit

loadSavedData :: forall eff. Eff (trace :: Trace, alert :: Alert, dom :: DOM, storage :: Storage | eff) Unit
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: F (Maybe FormData)
    savedData = do
      jsonOrNull <- read item
      -- read :: Foreign -> F FormData
      traverse readJSON (runNull jsonOrNull)
      -- traverse :: (String -> F FormData) -> Null String -> F (Maybe FormData)

  case savedData of
    Left err -> alert $ "Unable to read saved form data: " ++ show err
    Right Nothing -> return unit
    Right (Just (FormData o)) -> do
      updateForm "#inputFirstName" o.firstName
      updateForm "#inputLastName"  o.lastName

      updateForm "#inputStreet"    o.street
      updateForm "#inputCity"      o.city
      updateForm "#inputState"     o.state

      updateForm "#inputHomePhone" o.homePhone
      updateForm "#inputCellPhone" o.cellPhone

      return unit

validateAndSaveEntry :: forall eff. Eff (trace :: Trace, alert :: Alert, dom :: DOM, storage :: Storage | eff) Unit
validateAndSaveEntry = do
  trace "Running validators"

  errorsOrResult <- validateControls

  runV f g errorsOrResult
  return unit
    where
    f errs = alert $ "There are " ++ show (length errs) ++ " validation errors."
    g result = do
      setItem "person" $ stringify $ toForeign $ toFormData result
      alert "Saved"

main :: forall eff. Eff (trace :: Trace, alert :: Alert, dom :: DOM, storage :: Storage | eff) Unit
main = do
  trace "Loading data from local storage"
  loadSavedData

  trace "Attaching event handlers"
  setupEventHandlers

  Just saveButton <- querySelector "#saveButton"

  addEventListener "click" validateAndSaveEntry saveButton

  return unit
