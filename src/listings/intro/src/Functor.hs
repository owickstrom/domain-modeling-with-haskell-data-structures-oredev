{-# LANGUAGE NoImplicitPrelude  #-}
module Functor where

import           Prelude  hiding (Functor, Maybe (..), fmap)

-- start snippet functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-- end snippet functor

-- start snippet maybe
data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing
-- end snippet maybe
