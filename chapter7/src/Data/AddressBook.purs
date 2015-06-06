module Data.AddressBook where

import Data.Maybe
import Data.Traversable
import Data.Monoid
import Data.Foldable
import Control.Apply

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address
  { street:  street
  , city:    city
  , state:   state
  }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number = PhoneNumber
  { "type": ty
  , number: number
  }

newtype Person = Person
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  , phones    :: [PhoneNumber]
  }

person :: String -> String -> Address -> [PhoneNumber] -> Person
person firstName lastName address phones = Person
  { firstName: firstName
  , lastName:  lastName
  , address:   address
  , phones:    phones
  }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

instance showAddress :: Show Address where
  show (Address o) = "Address " ++
    "{ street: " ++ show o.street ++
    ", city: "   ++ show o.city ++
    ", state: "  ++ show o.state ++
    " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " ++
    "{ type: "   ++ show o."type" ++
    ", number: " ++ show o.number ++
    " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " ++
    "{ firstName: " ++ show o.firstName ++
    ", lastName: "  ++ show o.lastName ++
    ", address: "   ++ show o.address ++
    ", phones: "    ++ show o.phones ++
    " }"

combineArray :: forall f a. (Applicative f) => [f a] -> f [a]
combineArray [] = pure []
combineArray (x : xs) = (:) <$> x <*> combineArray xs

maybeAdd :: Maybe Number -> Maybe Number -> Maybe Number
maybeAdd = lift2 (+)

maybeSub :: Maybe Number -> Maybe Number -> Maybe Number
maybeSub = lift2 (-)

maybeMul :: Maybe Number -> Maybe Number -> Maybe Number
maybeMul = lift2 (*)

maybeDiv :: Maybe Number -> Maybe Number -> Maybe Number
maybeDiv = lift2 (/)

combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

traverse' :: forall a b f t. (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' f x = sequence $ f <$> x

sequence' :: forall a f t. (Applicative f, Traversable t) => t (f a) -> f (t a)
sequence' x = traverse id x

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  (<$>) _ Leaf = Leaf
  (<$>) f (Branch x y z) = Branch (f <$> x) (f y) (f <$> z)

instance foldableTree :: Foldable Tree where
  foldr f x (Branch l y r) = foldr f (f y (foldr f x r)) l
  foldr f x Leaf = x
  foldl f x (Branch l y r) = foldl f (f (foldl f x l) y) r
  foldl f x Leaf = x
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r
  foldMap f Leaf = mempty

instance inOrderTraverseTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch x y z) = Branch <$> traverse f x <*> f y <*> traverse f z
  sequence = traverse id

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch x y z) = "(Branch " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
