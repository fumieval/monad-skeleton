{-# LANGUAGE RankNTypes, GADTs, PolyKinds #-}
module Control.Monad.Undead (Zombie, necro, MonadView(..), rotten) where
import qualified Data.Sequence as Seq
import Unsafe.Coerce
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import GHC.Prim
import Prelude hiding (id, (.))

rotten :: Zombie t a -> MonadView t (Zombie t) a
rotten (Zombie (Return a) s) = case viewL s of
  Empty -> Return a
  Kleisli k :| c -> case k a of
    Zombie h t -> rotten $ Zombie h (c . t)
rotten (Zombie (t :>>= k) s) = t :>>= \a -> case k a of
  Zombie h t -> Zombie h (s . t)

necro :: t a -> Zombie t a
necro t = Zombie (t :>>= return) id

data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: t a -> (a -> m b) -> MonadView t m b

data Zombie t a where
  Zombie :: MonadView t (Zombie t) x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a

newtype Cat k a b = Cat (Seq.Seq Any)

data View j k a b where
  Empty :: View j k a a
  (:|) :: j a b -> k b c -> View j k a c

(|>) :: Cat k a b -> k b c -> Cat k a c
Cat s |> k = Cat (s Seq.|> unsafeCoerce k)

viewL :: Cat k a b -> View k (Cat k) a b
viewL (Cat s) = case Seq.viewl s of
  Seq.EmptyL -> unsafeCoerce Empty
  a Seq.:< b -> unsafeCoerce (:|) a b

instance Category (Cat k) where
  id = Cat Seq.empty
  Cat a . Cat b = Cat (b Seq.>< a)

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap

instance Monad (Zombie t) where
  return a = Zombie (Return a) id
  Zombie t c >>= k = Zombie t (c |> Kleisli k)
