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

-- | The spine of skeleta.
data Spine t m a where
  Spine :: MonadView t m a -> Cat (Kleisli m) a b -> Spine t m b

-- | Extend a spine.
graftSpine :: Cat (Kleisli m) a b -> Spine t m a -> Spine t m b
graftSpine c (Spine v d) = Spine v (Tree d c)
{-# INLINE graftSpine #-}

-- | 'Zombie' is a variant of 'Skeleton' which has an 'Alternative' instance.
data Zombie t a = Sunlight | Zombie (Spine t (Zombie t) a) (Zombie t a)

z_list :: Zombie t a -> [Spine t (Zombie t) a]
z_list Sunlight = []
z_list (Zombie x xs) = x : z_list xs

l_zombie :: [Spine t (Zombie t) a] -> Zombie t a
l_zombie [] = Sunlight
l_zombie (x : xs) = Zombie x (l_zombie xs)

z_app :: Zombie t a -> Zombie t a -> Zombie t a
z_app Sunlight ys = ys
z_app (Zombie x xs) ys = Zombie x (z_app xs ys)

z_map :: (Spine t (Zombie t) a -> Spine s (Zombie s) b) -> Zombie t a -> Zombie s b
z_map _ Sunlight = Sunlight
z_map f (Zombie x xs) = Zombie (f x) (z_map f xs)

zl_bind :: Zombie t a -> (Spine t (Zombie t) a -> [b]) -> [b]
zl_bind Sunlight _ = []
zl_bind (Zombie x xs) f = f x ++ zl_bind xs f

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
  return a = Zombie (Spine (Return a) id) Sunlight
  xs >>= k = z_map (graftSpine $ Leaf $ Kleisli k) xs

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm t = Zombie (Spine t id) Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm ss = zl_bind ss f where
  f (Spine v c) = case v of
    Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
      ss' -> disembalm $ z_map (graftSpine c') ss'
    t :>>= k -> return $ t :>>= \a -> case k a of
      ss' -> z_map (graftSpine c) ss'

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go ss = z_map (\(Spine v c) -> Spine (hoistMV f go v) (transCat (transKleisli go) c)) ss
{-# INLINE hoistZombie #-}
