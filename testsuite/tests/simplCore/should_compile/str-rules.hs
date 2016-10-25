{-# LANGUAGE MagicHash #-}
import GHC.CString (unpackFoldrCString#, unpackCString#)
import GHC.Base (eqString)
main :: IO ()
main = do
  let mix c n = fromEnum c + n
  n <- readLn
  print $
    unpackFoldrCString# "@@@ a"# mix
      (unpackFoldrCString# "b"# mix n)
  if eqString (unpackCString# "x"#) (unpackCString# "y"#)
    then putStrLn $ unpackCString# "@@@ c"#
    else putStrLn $ unpackCString# "@@@ d"#
  if eqString (unpackCString# "foo"#) (unpackCString# "foo"#)
    then putStrLn $ unpackCString# "@@@ e"#
    else putStrLn $ unpackCString# "@@@ f"#
