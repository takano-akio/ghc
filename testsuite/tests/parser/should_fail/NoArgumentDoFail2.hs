module NoArgumentDoFail2 where

forM a b = return ()

foo :: IO ()
foo = forM [1 .. 10] \x -> print x
