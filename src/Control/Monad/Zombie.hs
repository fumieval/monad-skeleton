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
newtype Zombie t a = Zombie { unZombie :: [Spine t (Zombie t) a] }

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Zombie []
  Zombie xs <|> Zombie ys = Zombie (xs ++ ys)

instance Monad (Zombie t) where
  return a = Zombie [Spine (Return a) id]
  Zombie xs >>= k = Zombie $ map (graftSpine $ Leaf $ Kleisli k) xs

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm t = Zombie [Spine t id]
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm (Zombie ss) = do
  Spine v c <- ss
  case v of
    Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
      Zombie ss' -> disembalm $ Zombie $ map (graftSpine c') ss'
    t :>>= k -> return $ t :>>= \a -> case k a of
      Zombie ss' -> Zombie $ map (graftSpine c) ss'

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go (Zombie ss) = Zombie [Spine (hoistMV f go v) (transCat (transKleisli go) c)
    | Spine v c <- ss]
{-# INLINE hoistZombie #-}
