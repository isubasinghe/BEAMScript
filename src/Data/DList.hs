{-# LANGUAGE RankNTypes #-}

module Data.DList where

data DList a = DList ([a] -> [a])

instance Show a => Show (DList a) where
  show x = show $ toList x

empty :: DList a
empty = DList id

singleton :: a -> DList a
singleton a = DList (a :)

append :: DList a -> DList a -> DList a
append (DList as) (DList bs) = DList (as . bs)

insert :: a -> DList a -> DList a
insert x xs = append xs (singleton x)

fromList :: [a] -> DList a
fromList as = DList (as ++)

toList :: DList a -> [a]
toList (DList as) = as []


