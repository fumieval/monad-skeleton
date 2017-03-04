module Control.Monad.Zombie where
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Skeleton
import Control.Monad.Skeleton.Internal
import Prelude hiding (id, (.))

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

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm t = Zombie [Spine t id]

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm (Zombie ss) = do
  Spine v c <- ss
  case v of
    Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
      Zombie ss' -> disembalm $ Zombie $ map (graftSpine c') ss'
    t :>>= k -> return $ t :>>= \a -> case k a of
      Zombie ss' -> Zombie $ map (graftSpine c) ss'
