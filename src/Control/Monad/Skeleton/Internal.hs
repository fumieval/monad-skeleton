{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
module Control.Monad.Skeleton.Internal (Cat(..), transCat, (|>), viewL, transKleisli) where

import Control.Arrow
import Unsafe.Coerce

data Cat k a b where
  Leaf :: k a b -> Cat k a b
  Tree :: Cat k a b -> Cat k b c -> Cat k a c

transCat :: (forall x y. j x y -> k x y) -> Cat j a b -> Cat k a b
transCat f (Tree a b) = transCat f a `Tree` transCat f b
transCat f (Leaf k) = Leaf (f k)
{-# INLINE transCat #-}

(|>) :: Cat k a b -> k b c -> Cat k a c
s |> k = Tree s (Leaf k)
{-# INLINE (|>) #-}

viewL :: forall k a b r. Cat k a b
  -> (k a b -> r)
  -> (forall x. k a x -> Cat k x b -> r)
  -> r
viewL (Leaf k) e _ = e k
viewL (Tree a b) _ r = go a b where
  go :: Cat k a x -> Cat k x b -> r
  go (Leaf k) t = r k t
  go (Tree c d) t = go c (Tree d t)

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f = unsafeCoerce (f Prelude..)
{-# INLINE transKleisli #-}
