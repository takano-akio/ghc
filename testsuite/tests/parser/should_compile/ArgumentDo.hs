{-# LANGUAGE ArgumentDo #-}

module ArgumentDo where

when :: Bool -> IO () -> IO ()
when True a = a
when False _ = return ()

forM xs f =
  case xs of
    [] -> return []
    (x:xs') -> do
      y <- f x
      ys <- forM xs' f
      return (y : ys)


foo :: IO ()
foo = when True do
  return ()

foo' :: IO ()
foo' = do
  forM [1 .. 10] \x ->
    print x

  forM [1 .. 10] \x -> do
    print x
    print x

  return ()

foo'' :: IO ()
foo'' = when
  do True
  do return ()
