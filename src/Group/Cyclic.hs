{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cyclic (
  Cyclic (Cyclic),
  display
  ) where

import Group

import Data.Proxy
import GHC.TypeLits (KnownNat, Nat, natVal)

-- | (Z, +) Integers with addition
instance Monoid Integer where
  mempty = 0
  mappend = (+)

instance Group Integer where
  inverse x = (- x)

instance AbelianGroup Integer
instance CountableGroup Integer

-- | (Zn, +) Integers with addition modulo n
newtype Cyclic (n :: Nat)
  = Cyclic Integer
  deriving (Eq, Read)

instance (KnownNat n) => Show (Cyclic n) where
  show x = display x

instance (KnownNat n) => Enum (Cyclic n) where
  toEnum i = (Cyclic (toInteger i))
  fromEnum (Cyclic i) = fromIntegral i

instance (KnownNat n) => Bounded (Cyclic n) where
  minBound = (Cyclic 0)
  maxBound = (Cyclic (natVal (Proxy :: Proxy n) - 1))

instance (KnownNat n) => Monoid (Cyclic n) where
  mempty = Cyclic 0
  mappend (Cyclic i) (Cyclic j) = Cyclic $ mod (i + j) (natVal (Proxy :: Proxy n))

instance (KnownNat n) => Group (Cyclic n) where
  inverse (Cyclic i) = (Cyclic $ (natVal (Proxy :: Proxy n)) - i)

instance (KnownNat n) => AbelianGroup (Cyclic n)
instance (KnownNat n) => CountableGroup (Cyclic n)
instance (KnownNat n) => FiniteGroup (Cyclic n)

display :: forall n . (KnownNat n) => Cyclic n -> String
display (Cyclic i) = "Z/" ++ show (natVal (Proxy :: Proxy n)) ++ " " ++ show i
