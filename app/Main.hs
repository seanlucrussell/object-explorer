module Main where

import Control.Exception (Exception, IOException, catch)
import Parsers (parseObject, repoSummaryParser)
import Renderers (renderObject, renderSummary)
import System.Environment
import System.Exit (exitFailure)
import System.Process (readProcess)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Parsec (ParseError, parse)
import Types (Object, ObjectSummary (hash))

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

output = "./generated/"

grabObject :: String -> ObjectSummary -> IO (Either ParseError Object)
grabObject repo o = do
  o <- catObject repo (hash o)
  return (parse parseObject "" o)

printObjectFromSummary :: String -> ObjectSummary -> IO ()
printObjectFromSummary repo s = do
  let ref = hash s
  o <- grabObject repo s
  case o of
    Left pe -> putStrLn ("Failed to read " ++ ref)
    Right ob ->
      writeFile (output ++ ref ++ ".html") (renderHtml (renderObject ref ob))

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Error: this script requires the path to a git repo as input"
      exitFailure
    else do
      let repo = head args
      s <-
        catch
          (listAllObjects repo)
          ( (const :: (IO String -> IOException -> IO String))
              ( do
                  putStrLn $ "Can't read git data from \"" ++ repo ++ "\""
                  exitFailure
              )
          )
      let repoSummary = parse repoSummaryParser "" s
      case repoSummary of
        Left _ -> putStrLn "Error generating list of all objects in repo"
        Right summary -> do
          print summary
          writeFile (output ++ "overview.html") (renderHtml (renderSummary summary))
          mapM_ (printObjectFromSummary repo) summary

-- tooltips: match output of git commands as closely as possible except for header info, add tooltips to provide more detail.

-- choose generated path
-- don't worry about files being well formed, just print error for hash (parse failed, content of body) and continue on