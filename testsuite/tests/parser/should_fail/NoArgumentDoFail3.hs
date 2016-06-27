{-# LANGUAGE LambdaCase #-}
module NoArgumentDoFail3 where

forM a b = return ()

foo :: IO ()
foo = forM [1 .. 10] \case
  Just 3 -> print x
