{-# LANGUAGE Trustworthy, RankNTypes, GADTs, PolyKinds #-}
module Control.Monad.Skeleton (MonadView(..)
  , hoistMV
  , iterMV
  , Skeleton
  , bone
  , debone
  , unbone
  , boned
  , hoistSkeleton) where
import qualified Data.Sequence as Seq
import Unsafe.Coerce
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import GHC.Prim
import Prelude hiding (id, (.))

-- | Re-add a bone.
boned :: MonadView t (Skeleton t) a -> Skeleton t a
boned t = Skeleton t id
{-# INLINE boned #-}

-- | Pick a bone from a 'Skeleton'.
debone :: Skeleton t a -> MonadView t (Skeleton t) a
debone (Skeleton (Return a) s) = case viewL s of
  Empty -> Return a
  Kleisli k :| c -> case k a of
    Skeleton h t -> debone $ Skeleton h (c . t)
debone (Skeleton (t :>>= k) s) = t :>>= \a -> case k a of
  Skeleton h t -> Skeleton h (s . t)

-- | Uncommon synonym for 'debone'.
unbone :: Skeleton t a -> MonadView t (Skeleton t) a
unbone = debone
{-# INLINE unbone #-}

-- | A skeleton that has only one bone.
bone :: t a -> Skeleton t a
bone t = Skeleton (t :>>= return) id
{-# INLINABLE bone #-}

-- | Lift a transformation between bones into transformation between skeletons.
hoistSkeleton :: (forall x. s x -> t x) -> Skeleton s a -> Skeleton t a
hoistSkeleton f (Skeleton v c) = Skeleton (hoistMV f (hoistSkeleton f) v)
  (transCat (transKleisli (hoistSkeleton f)) c)

data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: t a -> (a -> m b) -> MonadView t m b
infixl 1 :>>=

instance Functor m => Functor (MonadView t m) where
  fmap f (Return a) = Return (f a)
  fmap f (t :>>= k) = t :>>= fmap f . k
  {-# INLINE fmap #-}

hoistMV :: (forall x. s x -> t x) -> (m a -> n a) -> MonadView s m a -> MonadView t n a
hoistMV _ _ (Return a) = Return a
hoistMV f g (t :>>= k) = f t :>>= g . k
{-# INLINE hoistMV #-}

iterMV :: Monad m => (t a -> MonadView m t a) -> t a -> m a
iterMV f = go where
  go t = case f t of
    m :>>= k -> m >>= go . k
    Return a -> return a
{-# INLINE iterMV #-}

-- | @'Skeleton' t@ is a monadic skeleton (operational monad) made out of 't'.
-- Skeletons can be fleshed out by getting transformed to other monads.
-- The implementation is based on
-- <http://wwwhome.cs.utwente.nl/~jankuper/fp-dag/pref.pdf Reflection without Remorse>
-- so it provides efficient ('>>=') and 'debone', monadic reflection.
data Skeleton t a where
  Skeleton :: MonadView t (Skeleton t) x -> Cat (Kleisli (Skeleton t)) x a -> Skeleton t a

newtype Cat k a b = Cat (Seq.Seq Any)

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f = unsafeCoerce (f.)
{-# INLINE transKleisli #-}

transCat :: (forall x y. j x y -> k x y) -> Cat j a b -> Cat k a b
transCat f (Cat s) = Cat (fmap (unsafeCoerce f) s)
{-# INLINE transCat #-}

data View j k a b where
  Empty :: View j k a a
  (:|) :: j a b -> k b c -> View j k a c

(|>) :: Cat k a b -> k b c -> Cat k a c
Cat s |> k = Cat (s Seq.|> unsafeCoerce k)
{-# INLINE (|>) #-}

viewL :: Cat k a b -> View k (Cat k) a b
viewL (Cat s) = case Seq.viewl s of
  Seq.EmptyL -> unsafeCoerce Empty
  a Seq.:< b -> unsafeCoerce (:|) a b
{-# INLINE viewL #-}

instance Category (Cat k) where
  id = Cat Seq.empty
  {-# INLINE id #-}
  Cat a . Cat b = Cat (b Seq.>< a)
  {-# INLINE (.) #-}

instance Functor (Skeleton t) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Skeleton t) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Skeleton t) where
  return a = Skeleton (Return a) id
  {-# INLINE return #-}
  Skeleton t c >>= k = Skeleton t (c |> Kleisli k)
  {-# INLINE (>>=) #-}
