{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (Exception, IOException, catch)
import Language.Haskell.TH (Q, runIO, stringE)
import Parsers (branchesParser, parseObject, repoSummaryParser)
import Renderers (renderObject, renderSummary)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (createProcess, proc, readProcess)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Parsec (ParseError, parse)
import Types (Object, ObjectSummary (hash), RepoSummary (RepoSummary))

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

output :: [Char]
output = "generated/"

style :: [Char]
style =
  $( do
       s <- runIO (readFile "resources/style.css")
       stringE s
   )

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
      createProcess (proc "mkdir" [output])
      s <-
        catch
          (listAllObjects repo)
          ( (const :: (IO String -> IOException -> IO String))
              ( do
                  putStrLn $ "Can't read git data from \"" ++ repo ++ "\""
                  exitFailure
              )
          )
      head <- readProcess "git" ["rev-parse", "HEAD"] []
      branchesRaw <- readProcess "git" ["branch", "--format=%(objectname) %(refname:lstrip=-1)"] []
      case ( do
               summaryParse <- parse repoSummaryParser "" s
               branchesParse <- parse branchesParser "" branchesRaw
               return (summaryParse, branchesParse)
           ) of
        Left _ -> putStrLn "Error generating list of all objects in repo"
        Right (summary, branches) -> do
          let fullSummary = RepoSummary summary head branches
          putStrLn ("Generating git object overview for repo \"" ++ repo ++ "\" in \"" ++ output ++ "\"")
          writeFile (output ++ "overview.html") (renderHtml (renderSummary fullSummary))
          writeFile (output ++ "style.css") style
          mapM_ (printObjectFromSummary repo) summary
          putStrLn ("Generation complete. Look at \"" ++ output ++ "overview.html\" for the overview of the repo.")