{-# LANGUAGE PolyKinds, GADTs, Rank2Types, ScopedTypeVariables, Trustworthy #-}
module Control.Monad.Zombie.Internal (Cat(..), transCat, (|>), viewL) where

import Control.Category

data Cat k a b where
  Empty :: Cat k a a
  Leaf :: k a b -> Cat k a b
  Tree :: Cat k a b -> Cat k b c -> Cat k a c

transCat :: (forall x y. j x y -> k x y) -> Cat j a b -> Cat k a b
transCat f (Tree a b) = transCat f a `Tree` transCat f b
transCat f (Leaf k) = Leaf (f k)
transCat _ Empty = Empty
{-# INLINE transCat #-}

(|>) :: Cat k a b -> k b c -> Cat k a c
s |> k = Tree s (Leaf k)
{-# INLINE (|>) #-}

viewL :: forall k a b r. Cat k a b
  -> ((a ~ b) => r)
  -> (forall x. k a x -> Cat k x b -> r)
  -> r
viewL Empty e _ = e
viewL (Leaf k) _ r = k `r` Empty
viewL (Tree a b) e r = go a b where
  go :: Cat k a x -> Cat k x b -> r
  go Empty t = viewL t e r
  go (Leaf k) t = r k t
  go (Tree c d) t = go c (Tree d t)

instance Category (Cat k) where
  id = Empty
  {-# INLINE id #-}
  (.) = flip Tree
  {-# INLINE (.) #-}
