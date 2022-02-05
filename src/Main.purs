module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

newtype ReaderT :: ∀ k. Type -> (k -> Type) -> k -> Type
newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: ∀ r m a. ReaderT r m a -> r -> m a
runReaderT (ReaderT f) = f

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map f (ReaderT g) = ReaderT \r -> f <$> g r

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT f) (ReaderT g) = ReaderT \r -> f r <*> g r

instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

-- instance bindReaderT :: Bind m => Bind (ReaderT r m) where
--   bind (ReaderT f) g = ReaderT \r -> do
--      x <- f r
--      runReaderT (g x) r

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  bind (ReaderT f) g = ReaderT \r -> f r >>= \x -> runReaderT (g x) r

instance monadReaderT :: Monad m => Monad (ReaderT r m)

main :: Effect Unit
main = do
  log "🍝"
