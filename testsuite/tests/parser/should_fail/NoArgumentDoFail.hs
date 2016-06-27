module NoArgumentDoFail where

when :: Bool -> IO () -> IO ()
when True a = a
when False _ = return ()

foo :: IO ()
foo = when True do
  return ()
