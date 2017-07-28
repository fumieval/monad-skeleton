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
  ReturnZ :: x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a
  BindZ :: t y -> (y -> Zombie t x) -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a

zg_map :: Cat (Kleisli (Zombie t)) a b -> Zombie t a -> Zombie t b
zg_map _ Sunlight = Sunlight
zg_map f (ReturnZ x c xs) = ReturnZ x (Tree c f) (zg_map f xs)
zg_map f (BindZ y z c xs) = BindZ y z (Tree c f) (zg_map f xs)

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  Sunlight <|> ys = ys
  ReturnZ x c xs <|> ys = ReturnZ x c (xs <|> ys)
  BindZ y z c xs <|> ys = BindZ y z c (xs <|> ys)

instance Monad (Zombie t) where
  return a = ReturnZ a id Sunlight
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
embalm (Return x) = ReturnZ x id Sunlight
embalm (y :>>= z) = BindZ y z id Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm Sunlight = []
disembalm (ReturnZ x c xs) = disembalmR x c ++ disembalm xs
disembalm (BindZ y z c xs) = disembalmB y z c ++ disembalm xs

disembalmR :: a -> Cat (Kleisli (Zombie t)) a b -> [MonadView t (Zombie t) b]
disembalmR a c = viewL c [Return a] $ \(Kleisli k) c' -> case k a of
  ss' -> disembalm $ zg_map c' ss'

disembalmB :: t a -> (a -> Zombie t b) -> Cat (Kleisli (Zombie t)) b c -> [MonadView t (Zombie t) c]
disembalmB t k c = return $ t :>>= \a -> case k a of
  ss' -> zg_map c ss'

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (ReturnZ x c xs) = ReturnZ x (transCat (transKleisli go) c) (go xs)
  go (BindZ y z c xs) = BindZ (f y) (go . z) (transCat (transKleisli go) c) (go xs)
{-# INLINE hoistZombie #-}
