module Data.AddressBook.Validation where

import Data.Array
import Data.Maybe
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

type Errors = [String]

arrayNonEmpty :: forall a. String -> [a] -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" ++ field ++ "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Number -> String -> V Errors Unit
lengthIs field len value | S.length value /= len = invalid ["Field '" ++ field ++ "' must have length " ++ show len]
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

-- Exercise 2.1
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

-- Exercise 2.2
nonEmptyRegex :: R.Regex
nonEmptyRegex =
  R.regex
    "[^\\s]"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field s | R.test nonEmptyRegex s = pure unit
nonEmpty field _ = invalid ["Field '" ++ field ++ "' cannot be empty"]

matches :: String -> R.Regex -> String -> V Errors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _     = invalid ["Field '" ++ field ++ "' did not match the required format"]

validateAddress :: Address -> V Errors Address 
validateAddress (Address o) = 
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (matches  "State" stateRegex o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) = 
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> (traverse validateAddress o.address)
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = runV Left Right $ validatePerson p


-- Exercise 1.1
liftedPlus :: forall f. (Apply f) => f Number -> f Number -> f Number
liftedPlus = lift2 (+)

liftedMinus :: forall f. (Apply f) => f Number -> f Number -> f Number
liftedMinus = lift2 (-)

liftedMult :: forall f. (Apply f) => f Number -> f Number -> f Number
liftedMult = lift2 (*)

liftedDiv :: forall f. (Apply f) => f Number -> f Number -> f Number
liftedDiv = lift2 (/)


-- Exercise 1.3
combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing    = pure Nothing
combineMaybe (Just fx)  = Just <$> fx
