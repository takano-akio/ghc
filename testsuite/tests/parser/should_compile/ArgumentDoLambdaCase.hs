{-# LANGUAGE ArgumentDo, LambdaCase #-}

module ArgumentDoLambdaCase where

forM xs f =
  case xs of
    [] -> return []
    (x:xs') -> do
      y <- f x
      ys <- forM xs' f
      return (y : ys)

foo' :: IO ()
foo' = do
  forM [Just 3, Nothing] \case
    Just 3 -> print 3
    _ -> print 5

  return ()
