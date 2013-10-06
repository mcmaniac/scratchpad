{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Arbitrary where

type family IsEqual a b :: Bool where
  IsEqual a a = True
  IsEqual a b = False

type family Equality (k :: Bool) a b where
  Equality True  a b = (Eq a, a ~ b)
  Equality False a b = (a ~ a)

data Proxy (k :: Bool) = Proxy

mkProxy :: a -> b -> Proxy (IsEqual a b)
mkProxy _ _ = Proxy

class FallBack a where
  fallback :: a

class Arbitrary (k :: Bool) where
  liftArbitrary
    :: forall proxy a b c. (Equality k a b, FallBack c)
    => proxy k        -- Proxy, see `mkProxy`
    -> (a -> a -> c)  -- function to lift
    -> a
    -> b
    -> c

type IsArbitrary a b = (Equality (IsEqual a b) a b, Arbitrary (IsEqual a b))

instance Arbitrary True where
  liftArbitrary _proxy f a b = f a b

instance Arbitrary False where
  liftArbitrary _proxy _ _ _ = fallback

instance FallBack Bool where
  fallback = False

instance FallBack (Maybe a) where
  fallback = Nothing

isEqual :: (Eq a, IsArbitrary a b) => a -> b -> Bool
isEqual a b = liftArbitrary (mkProxy a b) (==) a b

isNotEqual :: (Eq a, IsArbitrary a b) => a -> b -> Bool
isNotEqual a b = not $ isEqual a b
