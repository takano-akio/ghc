{-# LANGUAGE LambdaCase #-}
module NoBlockArgumentsFail3 where

forM a b = return ()

foo :: IO ()
foo = forM [1 .. 10] \case
  Just 3 -> print x
