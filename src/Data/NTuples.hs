{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}

{- |
   Module      : Data.NTuples
   Description : Arbitrary-sized tuples
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : ivan.miljenovic@gmail.com



 -}
module Data.NTuples where

--------------------------------------------------------------------------------

type family ListFunction (ts :: [*]) (r :: *) :: * where
  ListFunction '[]       r =                      r
  ListFunction (t ': ts) r = t -> ListFunction ts r

newtype NTuple (ts :: [*]) = NTuple { unNTuple :: (forall r. ListFunction ts r -> r) }

tuple0 :: NTuple '[]
tuple0 = NTuple id

consN :: a -> NTuple ts -> NTuple (a ': ts)
consN a (NTuple f) = NTuple (f . ($a))

infixr 5 `consN`

unconsN :: NTuple (t ': ts) -> NTuple ts
unconsN (NTuple f) = NTuple (f . const)

-- snocN :: NTuple ts -> a -> NTuple (Snoc ts a)
-- snocN (NTuple f) a = NTuple (\g -> (g f) a)

type family Snoc (ts :: [*]) (a :: *) where
  Snoc '[]       a =              a ': '[]
  Snoc (t ': ts) a = t ': Snoc ts a

-- class WithArguments f r | f -> r where

--   addArg :: a -> (f -> r) -> ((a -> f) -> r)

-- instance {-# OVERLAPPABLE #-} WithArguments r r where

--   addArg :: a -> (r -> r) -> ((a -> r) -> r)
--   addArg a cont = (\g -> cont (g a))

-- instance (WithArguments f r) => WithArguments (t -> f) r where

--   addArg :: a -> (f -> r) -> ((a -> f) -> r)
--   addArg a cont = (\g -> cont (g a))
