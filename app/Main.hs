module Main where

import Control.Monad
import Parsers (parseObject, repoSummaryParser)
import System.Process (readProcess)
import Text.Parsec (parse)
import Types

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

repo = "../git-exploration"

grabObject o = do
  o <- catObject repo (hash o)
  return (parse parseObject "" o)

printObjectFromSummary s = do
  o <- grabObject s
  print o

main :: IO ()
main = do
  s <- listAllObjects repo
  let repoSummary = parse repoSummaryParser "" s
  case repoSummary of
    Left _ -> putStrLn "Error generating list of all objects in repo"
    Right summary -> do
      print summary
      mapM_ printObjectFromSummary summary