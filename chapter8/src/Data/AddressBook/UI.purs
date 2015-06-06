module Data.AddressBook.UI where

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Validation

import Data.Traversable

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM

import Debug.Trace

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

displayValidationErrors :: forall eff. [ValidationError] -> Eff (dom :: DOM, trace :: Trace | eff) Unit
displayValidationErrors errs = do
  foreachE errs $ \(ValidationError err field) -> do
    maybeNode <- querySelector $ fieldErrorSelector field
    case maybeNode of
      Just node -> do
        alert <- createElement "div" >>= setText err >>= addClass "small" >>= addClass "alert" >>= addClass "alert-danger"
        alert `appendChild` node
        return unit
      Nothing -> trace $ "Couldn't find " ++ (fieldErrorSelector field) ++ "."
  return unit

clearErrors :: forall e. Eff (dom :: DOM, trace :: Trace | e) [Unit]
clearErrors = traverse ((>>= maybeClear) <<< fieldErrorNode) allFields
  where
  maybeClear = maybe (return unit) clear
  clear node = setInnerHTML "" node >>= \_ -> return unit
  allFields = [ FirstNameField
              , LastNameField
              , StreetField
              , CityField
              , StateField
              , PhoneField HomePhone
              , PhoneField CellPhone
              , PhoneField WorkPhone
              , PhoneField OtherPhone
              ]

validateControls :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) (V Errors Person)
validateControls = do
  trace "Running validators"
  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           ]
  return $ validatePerson' p

fieldErrorSelector :: forall eff. Field -> String
fieldErrorSelector field = "#error" ++ show field

fieldErrorNode :: forall eff. Field -> Eff (trace :: Trace, dom :: DOM | eff) (Maybe Node)
fieldErrorNode field = querySelector selector >>= warnIfNothing
  where
  warnIfNothing (Just x) = return $ Just x
  warnIfNothing Nothing = trace ("Couldn't find \"" ++ selector ++ "\".") >>= \_ -> return Nothing
  selector = fieldErrorSelector field

validateAndUpdateUI :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  clearErrors
  errorsOrResult <- validateControls
  runV displayValidationErrors print errorsOrResult
  return unit

setupEventHandlers :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  body >>= addEventListener "change" validateAndUpdateUI

  return unit
