module Control.Monad.Extras where

mapM_ :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m Unit
mapM_ _ [] = return unit
mapM_ f (x : xs) = do
  f x
  mapM_ f xs
