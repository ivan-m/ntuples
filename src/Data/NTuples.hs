{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies,
             TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Data.NTuples
   Description : Arbitrary-sized tuples
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : ivan.miljenovic@gmail.com



 -}
module Data.NTuples where

import Data.Proxy   (Proxy(..))
import GHC.TypeLits

--------------------------------------------------------------------------------

type family ListFunction (ts :: [*]) (r :: *) :: * where
  ListFunction '[]       r =                      r
  ListFunction (t ': ts) r = t -> ListFunction ts r

newtype NTuple (ts :: [*]) = NTuple { unNTuple :: (forall r. ListFunction ts r -> r) }

uncurryN :: ListFunction ts r -> NTuple ts -> r
uncurryN = flip unNTuple

tuple0 :: NTuple '[]
tuple0 = NTuple id

consN :: a -> NTuple ts -> NTuple (a ': ts)
consN a (NTuple f) = NTuple (f . ($a))

infixr 5 `consN`

unconsN :: NTuple (t ': ts) -> NTuple ts
unconsN (NTuple f) = NTuple (f . const)

type family Length (ts :: [*]) :: Nat where
  Length '[] = 0
  Length (t ': ts) = Length ts + 1

-- -- | 1-based indexing
-- type family (ts :: [*]) !! (n :: Nat) :: * where
--   (t ': ts) !! 1 = t
--   (t ': ts) !! n = ts !! (n-1)
--   ts        !! n = TypeError ('Text "Either " ':<>: 'ShowType n ':<>: 'Text "> Length " ':<>: 'ShowType ts
--                               ':<>: 'Text " or is < 1.")

-- type family Tail (ts :: [*]) :: [*] where
--   Tail (t ': ts) = ts
--   Tail '[]       = TypeError ('Text "Cannot take the tail of an empty list.")

-- class (1 <= n, n <= Length ts) => HasN ts n where
--   nth :: Proxy n -> NTuple ts -> ts !! n

-- instance HasN '[t] 1 where
--   nth _ = uncurryN id

-- instance (1 <= n, HasN (Tail ts) (n-1)) => HasN ts n where
--   nth _ = nth (Proxy :: Proxy (n-1)) . unconsN

-- snocN :: forall ts a. NTuple ts -> a -> NTuple (Snoc ts a)
-- snocN nt a = NTuple (\(g :: ListFunction (Snoc ts a) r) -> (unNTuple nt :: ListFunction ts (a -> r)) g a)

type family Snoc (ts :: [*]) (a :: *) :: [*] where
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

--------------------------------------------------------------------------------

data HList (ts :: [*]) where
  HNil  :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- class Tupled
