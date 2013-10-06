{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Arbitrary
  ( -- * Arbitrary data type
    Arbitrary
  , liftArbitrary
  , Proxy, mkProxy
  , FallBack (..)
    -- * Examples
  , isEqual, isNotEqual
  , concatArbitrary
  , runExamples
  ) where

type family IsEqual a b :: Bool where
  IsEqual a a = True
  IsEqual a b = False

type family Equality (k :: Bool) a b where
  Equality True  a b = (a ~ b) -- set equality constraint
  Equality False a b = (a ~ a) -- dummy, no equality given

data Proxy (k :: Bool) = Proxy

mkProxy :: a -> b -> Proxy (IsEqual a b)
mkProxy _ _ = Proxy

class FallBack a where
  fallback :: a

class ArbitraryC (k :: Bool) where
  liftArbitrary
    :: forall proxy a b c. (Equality k a b, FallBack c)
    => proxy k        -- Proxy, see `mkProxy`
    -> (a -> a -> c)  -- function to lift
    -> a
    -> b
    -> c

instance ArbitraryC True where
  liftArbitrary _proxy f a b = f a b

instance ArbitraryC False where
  liftArbitrary _proxy _ _ _ = fallback

-- | Arbitrary argument type \"constraint\"
type Arbitrary a b = (Equality (IsEqual a b) a b, ArbitraryC (IsEqual a b))

instance FallBack Bool where
  fallback = False

-- | `(==)` lifted to arbitrary types, falling back to `False` on argument type
-- mismatch
isEqual :: (Eq a, Arbitrary a b) => a -> b -> Bool
isEqual a b = liftArbitrary (mkProxy a b) (==) a b

isNotEqual :: (Eq a, Arbitrary a b) => a -> b -> Bool
isNotEqual a b = not $ isEqual a b

instance FallBack (Maybe a) where
  fallback = Nothing

-- | `(++)` lifted to arbitrary types, falling back to `Nothing` on argument
-- type mismatch
concatArbitrary :: Arbitrary [a] [b] => [a] -> [b] -> Maybe [a]
concatArbitrary a b = liftArbitrary (mkProxy a b) (\c d -> Just $ c ++ d) a b

--------------------------------------------------------------------------------

runExamples :: IO ()
runExamples = do
  print $ isEqual "qwe" "qwe"                 -- True
  print $ isEqual "qwe" "asd"                 -- False
  print $ isEqual "qwe" (1 :: Int)            -- False

  print $ concatArbitrary "qwe" "asd"         -- Just "qweasd"
  print $ concatArbitrary "qwe" [1 :: Int]    -- Nothing
