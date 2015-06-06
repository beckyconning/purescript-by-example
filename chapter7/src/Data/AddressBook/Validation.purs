module Data.AddressBook.Validation where

import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

type Errors = [ValidationError]

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

data ValidationError = ValidationError String Field

instance showField :: Show Field where
  show FirstNameField         = "FirstName"
  show LastNameField          = "LastName"
  show StreetField            = "Street"
  show CityField              = "City"
  show StateField             = "State"
  show (PhoneField phoneType) = show phoneType

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" ++ show field ++ "' cannot be empty") field]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> [a] -> V Errors Unit
arrayNonEmpty field [] = invalid [ValidationError ("Field '" ++ show field ++ "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Number -> String -> V Errors Unit
lengthIs field len value | S.length value /= len = invalid [ValidationError ("Field '" ++ show field ++ "' must have length " ++ show len) field]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: R.Regex
phoneNumberRegex =
  R.regex
    "^\\d{3}-\\d{3}-\\d{4}$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

stateRegex :: R.Regex
stateRegex =
  R.regex
    "^[A-z]{2}$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

nonEmptyRegex :: R.Regex
nonEmptyRegex =
  R.regex
    ".*\\S.*"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

matches :: Field -> R.Regex -> String -> V Errors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _     = invalid [ValidationError ("Field '" ++ show field ++ "' did not match the required format") field]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (matches StreetField nonEmptyRegex o.street *> pure o.street)
          <*> (matches CityField nonEmptyRegex o.city *> pure o.city)
          <*> (matches StateField stateRegex o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o.type) phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.address
         <*> (traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> V Errors Person
validatePerson' (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField o.lastName  *> pure o.lastName)
         <*> validateAddress o.address
         <*> (traverse validatePhoneNumber o.phones)
