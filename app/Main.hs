module Main where

import Parsers (parseObject, repoSummaryParser)
import System.Process (readProcess)
import Text.Parsec (parse)

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

main :: IO ()
main = do
  s <- listAllObjects "../git-exploration"
  putStrLn s
  print (parse repoSummaryParser "" s)
  t <- catObject "../git-exploration" "0b17c4a34f855a61f4e5fb4da600ce295360672b"
  putStrLn ""
  putStrLn t
  print (parse parseObject "" t)
  c <- catObject "../git-exploration" "b606b71986453d34f8665d14ad0d49398fd102fd"
  putStrLn ""
  putStrLn c
  print (parse parseObject "" c)