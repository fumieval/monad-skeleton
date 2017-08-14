{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs #-}
module Control.Monad.Zombie (Zombie(..)
  , liftZ
  , embalm
  , disembalm
  , disembalmBy
  , hoistZombie
  ) where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Skeleton
import Control.Monad.Skeleton.Internal
import Prelude hiding (id, (.))

-- | 'Zombie' is a variant of 'Skeleton' which has an 'Alternative' instance.
data Zombie t a where
  Sunlight :: Zombie t a
  ReturnZ :: a -> Zombie t a -> Zombie t a
  BindZ :: t x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  Sunlight <|> ys = ys
  ReturnZ x xs <|> ys = ReturnZ x (xs <|> ys)
  BindZ x c xs <|> ys = BindZ x c (xs <|> ys)

instance Monad (Zombie t) where
  return a = ReturnZ a Sunlight
  Sunlight >>= _ = Sunlight
  ReturnZ a xs >>= k = k a <|> (xs >>= k)
  BindZ x c xs >>= k = BindZ x (c |> Kleisli k) (xs >>= k)

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm (Return x) = ReturnZ x Sunlight
embalm (x :>>= k) = BindZ x (Leaf $ Kleisli k) Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm = disembalmBy [] (:)
{-# INLINE disembalm #-}

-- | Decompose a zombie as a list of possibilitie and fold this list.
disembalmBy :: r -> (MonadView t (Zombie t) a -> r -> r) -> Zombie t a -> r
disembalmBy e r = go where
  go Sunlight = e
  go (ReturnZ x xs) = Return x `r` go xs
  go (BindZ x c xs) = (x :>>= disembalm_go c) `r` go xs

disembalm_go :: Cat (Kleisli (Zombie t)) a b -> a -> Zombie t b
disembalm_go c a = viewL c (\(Kleisli k) -> k a) $
  \(Kleisli k) c' -> disembalm_go2 c' $ k a

disembalm_go2 :: Cat (Kleisli (Zombie t)) a b -> Zombie t a -> Zombie t b
disembalm_go2 c = go where
  go Sunlight = Sunlight
  go (ReturnZ a xs) = disembalm_go c a <|> go xs
  go (BindZ t d xs) = BindZ t (Tree d c) $ go xs

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (ReturnZ x xs) = ReturnZ x (go xs)
  go (BindZ x c xs) = BindZ (f x) (transCat (transKleisli go) c) (go xs)
{-# INLINE hoistZombie #-}
