monad-skeleton
======================

[![Build Status](https://travis-ci.org/fumieval/monad-skeleton.svg?branch=master)](https://travis-ci.org/fumieval/monad-skeleton)
[![Hackage](https://budueba.com/hackage/monad-skeleton)](https://hackage.haskell.org/package/monad-skeleton)

This package provides `Skeleton`, an operational monad. The internal encoding
gives O(1) bind and monadic reflection.

`Skeleton` promotes unit instructions to a monad. It is isomorphic to
`MonadView (Skeleton t)`:

```haskell
data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: !(t a) -> (a -> m b) -> MonadView t m b

boned :: MonadView t (Skeleton t) a -> Skeleton t a
debone :: Skeleton t a -> MonadView t (Skeleton t) a
```

GADTs are handy to define instructions:

```haskell
data Interaction x where
  Get :: Interacton String
  Put :: String -> Interaction ()

echo :: Skeleton Interaction ()
echo = bone Get >>= bone . Put
```

Use `debone` to interpret a computation.

```haskell
interpret :: Skeleton Interaction a -> IO a
interpret m = case debone m of
  Return a -> return a
  Get :>>= k -> getLine >>= interpret . k
  Put s :>>= k -> putStrLn s >>= interpret . k
```
