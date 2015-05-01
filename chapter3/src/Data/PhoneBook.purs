module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons

findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntries :: String -> String -> PhoneBook -> PhoneBook
findEntries firstName lastName = filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

reverseFindEntry :: String -> PhoneBook -> Maybe Entry
reverseFindEntry phone = head <<< filter hasThisPhoneNumber
  where
  hasThisPhoneNumber :: Entry -> Boolean
  hasThisPhoneNumber entry = entry.phone == phone

containsName :: String -> String -> PhoneBook -> Boolean
containsName firstName lastName = not <<< null <<< filter hasThisName
  where
  hasThisName :: Entry -> Boolean
  hasThisName entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates = nubBy entryEquality
  where
  entryEquality :: Entry -> Entry -> Boolean
  entryEquality x y = x.firstName == y.firstName && x.lastName == y.lastName && x.phone == y.phone
