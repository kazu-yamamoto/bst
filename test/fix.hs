module Main where

main :: IO ()
main = do
  cs <- getContents
  let ls = lines cs
      (ps,fl) = break (== "") ls
  writeFile "pass.dat" (unlines ps)
  writeFile "fail.dat" (unlines (tail fl))
