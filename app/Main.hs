module Main where

import Control.Monad
import Parsers (parseObject, repoSummaryParser)
import Renderers
import System.Process (readProcess)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Parsec (ParseError, parse)
import Types

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

--repo = "../git-exploration"

repo = "."

output = "./generated/"

grabObject :: ObjectSummary -> IO (Either ParseError Object)
grabObject o = do
  o <- catObject repo (hash o)
  return (parse parseObject "" o)

printObjectFromSummary :: ObjectSummary -> IO ()
printObjectFromSummary s = do
  let ref = hash s
  o <- grabObject s
  case o of
    Left pe -> putStrLn ("Failed to read " ++ ref)
    Right ob -> writeFile (output ++ ref ++ ".html") (renderHtml (renderObject ref ob))

main :: IO ()
main = do
  s <- listAllObjects repo
  let repoSummary = parse repoSummaryParser "" s
  case repoSummary of
    Left _ -> putStrLn "Error generating list of all objects in repo"
    Right summary -> do
      print summary
      writeFile (output ++ "overview.html") (renderHtml (renderSummary summary))
      mapM_ printObjectFromSummary summary

-- tooltips: match output of git commands as closely as possible except for header info, add tooltips to provide more detail.
-- title is hash
-- Object <hash>
-- Preformatted for blob
-- validate repo exists/git is installed
-- don't worry about files being well formed, just print error for hash (parse failed, content of body) and continue on