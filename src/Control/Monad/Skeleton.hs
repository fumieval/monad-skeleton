{-# LANGUAGE RankNTypes, GADTs, PolyKinds #-}
module Control.Monad.Skeleton (MonadView(..), hoistMonadView, Skeleton, bone, unbone, hoistSkeleton) where
import qualified Data.Sequence as Seq
import Unsafe.Coerce
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import GHC.Prim
import Prelude hiding (id, (.))

unbone :: Skeleton t a -> MonadView t (Skeleton t) a
unbone (Skeleton (Return a) s) = case viewL s of
  Empty -> Return a
  Kleisli k :| c -> case k a of
    Skeleton h t -> unbone $ Skeleton h (c . t)
unbone (Skeleton (t :>>= k) s) = t :>>= \a -> case k a of
  Skeleton h t -> Skeleton h (s . t)

bone :: t a -> Skeleton t a
bone t = Skeleton (t :>>= return) id
{-# INLINABLE bone #-}

hoistSkeleton :: (forall x. s x -> t x) -> Skeleton s a -> Skeleton t a
hoistSkeleton f (Skeleton v c) = Skeleton (hoistMonadView f (hoistSkeleton f) v)
  (transCat (transKleisli (hoistSkeleton f)) c)

data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: t a -> (a -> m b) -> MonadView t m b
infixl 1 :>>=

hoistMonadView :: (forall x. s x -> t x) -> (m a -> n a) -> MonadView s m a -> MonadView t n a
hoistMonadView _ _ (Return a) = Return a
hoistMonadView f g (t :>>= k) = f t :>>= g . k

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
