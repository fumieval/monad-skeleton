{-# LANGUAGE BangPatterns, Trustworthy, RankNTypes, GADTs, ScopedTypeVariables #-}
module Control.Monad.Skeleton (MonadView(..)
  , hoistMV
  , iterMV
  , Skeleton(..)
  , bone
  , debone
  , unbone
  , boned
  , hoistSkeleton
  , Spine(..)
  ) where
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Category
import Unsafe.Coerce
import Control.Monad.Skeleton.Internal
import Prelude hiding (id, (.))

-- | Re-add a bone.
boned :: MonadView t (Skeleton t) a -> Skeleton t a
boned t = Skeleton (Spine t id)
{-# INLINE boned #-}

-- | Pick a bone from a 'Skeleton'.
debone :: Skeleton t a -> MonadView t (Skeleton t) a
debone (Skeleton (Spine v s)) = case v of
  Return a -> viewL s (Return a) $ \(Kleisli k) c -> case k a of
    Skeleton (Spine h t) -> debone $ Skeleton $ Spine h (c . t)
  t :>>= k -> t :>>= \a -> case k a of
    Skeleton (Spine h c) -> Skeleton (Spine h (s . c))

-- | Uncommon synonym for 'debone'.
unbone :: Skeleton t a -> MonadView t (Skeleton t) a
unbone = debone
{-# INLINE unbone #-}

-- | A skeleton that has only one bone.
bone :: t a -> Skeleton t a
bone t = Skeleton (Spine (t :>>= return) id)
{-# INLINABLE bone #-}

-- | Lift a transformation between bones into transformation between skeletons.
hoistSkeleton :: forall s t a. (forall x. s x -> t x) -> Skeleton s a -> Skeleton t a
hoistSkeleton f = go where
  go :: forall x. Skeleton s x -> Skeleton t x
  go (Skeleton (Spine v c)) = Skeleton $ Spine (hoistMV f go v)
    (transCat (transKleisli go) c)
{-# INLINE hoistSkeleton #-}

-- | A deconstructed action
data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: !(t a) -> (a -> m b) -> MonadView t m b
infixl 1 :>>=

instance Functor m => Functor (MonadView t m) where
  fmap f (Return a) = Return (f a)
  fmap f (t :>>= k) = t :>>= fmap f . k
  {-# INLINE fmap #-}

-- | Transform the action and the continuation.
hoistMV :: (forall x. s x -> t x) -> (m a -> n a) -> MonadView s m a -> MonadView t n a
hoistMV _ _ (Return a) = Return a
hoistMV f g (t :>>= k) = f t :>>= g . k
{-# INLINE hoistMV #-}

-- | Join 'MonadView' recursively.
iterMV :: Monad m => (t a -> MonadView m t a) -> t a -> m a
iterMV f = go where
  go t = case f t of
    m :>>= k -> m >>= go . k
    Return a -> return a
{-# INLINE iterMV #-}

data Spine t m a where
  Spine :: MonadView t m a -> Cat (Kleisli m) a b -> Spine t m b

-- | @'Skeleton' t@ is a monadic skeleton (operational monad) made out of 't'.
-- Skeletons can be fleshed out by getting transformed to other monads.
-- It provides O(1) ('>>=') and 'debone', the monadic reflection.
newtype Skeleton t a = Skeleton { unSkeleton :: Spine t (Skeleton t) a }

instance Functor (Skeleton t) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Skeleton t) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  (*>) = (>>)
  {-# INLINE (*>) #-}
  a <* b = a >>= \x -> b >> return x

instance Monad (Skeleton t) where
  return a = Skeleton $ Spine (Return a) id
  Skeleton (Spine t c) >>= k = Skeleton $ Spine t (c |> Kleisli k)

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f = unsafeCoerce (f.)
{-# INLINE transKleisli #-}
