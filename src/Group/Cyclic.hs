{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cyclic (
  Cyclic (Cyclic)
  ) where

import Group

import Data.Proxy
import GHC.TypeLits

-- | (Z, +) Integers with addition
instance Group Integer where
  identity = 0
  operation = (+)
  inverse x = (- x)

instance AbelianGroup Integer
instance CountableGroup Integer

-- | (Zn, +) Integers with addition modulo n
newtype Cyclic (n :: Nat)
  = Cyclic Integer
  deriving (Read, Show, Eq)

-- instance (KnownNat n) => Show (Cyclic n) where
--   show (Cyclic i) = show i
--   show (Cyclic i) = "Z/" ++ show (natVal (Proxy :: Proxy n)) ++ " " ++ show i

instance (KnownNat n) => Enum (Cyclic n) where
  toEnum i = (Cyclic (toInteger i))
  fromEnum (Cyclic i) = fromIntegral i

instance (KnownNat n) => Bounded (Cyclic n) where
  minBound = (Cyclic 0)
  maxBound = (Cyclic (natVal (Proxy :: Proxy n) - 1))

instance (KnownNat n) => Group (Cyclic n) where
  identity = Cyclic 0
  operation (Cyclic i) (Cyclic j) = Cyclic $ mod (i + j) (natVal (Proxy :: Proxy n))
  inverse (Cyclic i) = (Cyclic $ (natVal (Proxy :: Proxy n)) - i)

instance (KnownNat n) => AbelianGroup (Cyclic n)
instance (KnownNat n) => CountableGroup (Cyclic n)
instance (KnownNat n) => FiniteGroup (Cyclic n)
