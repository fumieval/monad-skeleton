{-# LANGUAGE BangPatterns, RankNTypes, GADTs, ScopedTypeVariables #-}
module Control.Monad.Skeleton (MonadView(..)
  , hoistMV
  , iterMV
  , Skeleton(..)
  , bone
  , debone
  , unbone
  , boned
  , hoistSkeleton
  ) where
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Monad.Skeleton.Internal
import Prelude hiding (id, (.))

-- | Re-add a bone.
boned :: MonadView t (Skeleton t) a -> Skeleton t a
boned (Return a) = ReturnS a
boned (t :>>= k) = BindS t $ Leaf $ Kleisli k
{-# INLINE boned #-}

-- | Pick a bone from a 'Skeleton'.
debone :: Skeleton t a -> MonadView t (Skeleton t) a
debone (ReturnS a) = Return a
debone (BindS t c0) = t :>>= go c0 where
  go :: Cat (Kleisli (Skeleton t)) a b -> a -> Skeleton t b
  go c a = viewL c (ReturnS a) $ \(Kleisli k) c' -> case k a of
    ReturnS b -> go c' b
    BindS t' c'' -> BindS t' (c' . c'')

-- | Uncommon synonym for 'debone'.
unbone :: Skeleton t a -> MonadView t (Skeleton t) a
unbone = debone
{-# INLINE unbone #-}
{-# DEPRECATED unbone "Use debone instead" #-}

-- | A skeleton that has only one bone.
bone :: t a -> Skeleton t a
bone t = BindS t id
{-# INLINABLE bone #-}

-- | Lift a transformation between bones into transformation between skeletons.
hoistSkeleton :: forall s t a. (forall x. s x -> t x) -> Skeleton s a -> Skeleton t a
hoistSkeleton f = go where
  go :: forall x. Skeleton s x -> Skeleton t x
  go (ReturnS a) = ReturnS a
  go (BindS t c) = BindS (f t) $ transCat (transKleisli go) c
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

-- | @'Skeleton' t@ is a monadic skeleton (operational monad) made out of 't'.
-- Skeletons can be fleshed out by getting transformed to other monads.
-- It provides O(1) ('>>=') and 'debone', the monadic reflection.
data Skeleton t a where
  ReturnS :: a -> Skeleton t a
  BindS :: t a -> Cat (Kleisli (Skeleton t)) a b -> Skeleton t b

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
  return = ReturnS
  ReturnS a >>= k = k a
  BindS t c >>= k = BindS t (c |> Kleisli k)
