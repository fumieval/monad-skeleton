{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs #-}
module Control.Monad.Zombie where
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Skeleton
import Control.Monad.Skeleton.Internal (transKleisli)
import Control.Monad.Zombie.Internal
import Prelude hiding (id, (.))

-- | 'Zombie' is a variant of 'Skeleton' which has an 'Alternative' instance.
data Zombie t a where
  Sunlight :: Zombie t a
  Zombie :: MonadView t (Zombie t) x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a

z_app :: Zombie t a -> Zombie t a -> Zombie t a
z_app Sunlight ys = ys
z_app (Zombie v c xs) ys = Zombie v c (z_app xs ys)

zg_map :: Cat (Kleisli (Zombie t)) a b -> Zombie t a -> Zombie t b
zg_map _ Sunlight = Sunlight
zg_map f (Zombie v c xs) = Zombie v (Tree c f) (zg_map f xs)

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  xs <|> ys = z_app xs ys

instance Monad (Zombie t) where
  return a = Zombie (Return a) id Sunlight
  xs >>= k = zg_map (Leaf $ Kleisli k) xs

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm t = Zombie t id Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm Sunlight = []
disembalm (Zombie v c xs) = disembalmF v c ++ disembalm xs

disembalmF :: MonadView t (Zombie t) a -> Cat (Kleisli (Zombie t)) a b -> [MonadView t (Zombie t) b]
disembalmF v c = case v of
  Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
    ss' -> disembalm $ zg_map c' ss'
  t :>>= k -> return $ t :>>= \a -> case k a of
    ss' -> zg_map c ss'

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (Zombie v c xs) = Zombie (hoistMV f go v) (transCat (transKleisli go) c) (go xs)
{-# INLINE hoistZombie #-}
