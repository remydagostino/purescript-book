module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry e = e.lastName ++ ", " ++
              e.firstName ++ ": " ++
              e.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons

findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry first last = head <<< filter filterEntry
  where
    filterEntry = \e -> e.firstName == first && e.lastName == last

findEntryByPhone :: String -> PhoneBook -> Maybe Entry
findEntryByPhone phone = head <<< filter filterEntry
  where
    filterEntry = \e -> e.phone == phone

isNameInBook :: String -> String -> PhoneBook -> Boolean
isNameInBook first last = null <<< filter filterEntry
  where
    filterEntry = \e -> e.firstName == first && e.lastName == last

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates = nubBy (\a b -> a.firstName == b.firstName &&
                                  a.lastName  == b.lastName &&
                                  a.phone     == b.phone)
