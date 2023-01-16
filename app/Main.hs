module Main where

import Data.Functor (($>))
import qualified Data.Functor.Identity
import System.Process
import Text.Parsec
import Text.Parsec.Char

catObject :: String -> String -> IO String
catObject path hash = readProcess "git" ["-C", path, "cat-file", "-p", hash] []

listAllObjects :: String -> IO String
listAllObjects path = readProcess "git" ["-C", path, "cat-file", "--batch-check", "--batch-all-objects"] []

repoSummaryParser :: ParsecT String u Data.Functor.Identity.Identity [ObjectSummary]
repoSummaryParser = sepEndBy1 objectSummaryParser newline
  where
    objectSummaryParser = do
      hash <- parseHash
      spaces
      objectType <- objectTypeParser
      spaces
      byteCount <- read <$> many1 digit
      return (ObjectSummary hash objectType byteCount)

treeParser :: ParsecT String u Data.Functor.Identity.Identity [TreeEntry]
treeParser = sepEndBy1 treeEntryParser newline
  where
    treeEntryParser = do
      perms <- permsParser
      spaces
      objectType <- objectTypeParser
      spaces
      hash <- parseHash
      spaces
      fileName <- manyTill anyChar newline
      return (TreeEntry perms objectType hash fileName)

commitParser :: ParsecT String u Data.Functor.Identity.Identity Commit
commitParser = do
  string "tree"
  space
  treeHash <- parseHash
  newline
  parents <-
    sepEndBy1
      ( do
          string "parent"
          space
          parseHash
      )
      newline
  string "author"
  spaces
  authorName <- manyTill anyChar (char '<')
  authorEmail <- manyTill anyChar (char '>')
  spaces
  authorTime <- read <$> many1 digit
  spaces
  authorTimezone <- manyTill anyChar newline
  string "committer"
  spaces
  committerName <- manyTill anyChar (char '<')
  committerEmail <- manyTill anyChar (char '>')
  spaces
  committerTime <- read <$> many1 digit
  spaces
  committerTimezone <- manyTill anyChar newline
  newline
  message <- many1 anyChar
  return
    ( Commit
        treeHash
        parents
        (UserInfo authorName authorEmail authorTime authorTimezone)
        (UserInfo committerName committerEmail committerTime committerTimezone)
        message
    )

parseHash = many1 (letter <|> digit)

match :: Monad m => String -> a -> ParsecT String u m a
match a b = try (string a $> b)

objectTypeParser :: Monad m => ParsecT String u m ObjectType
objectTypeParser = choice [match "tree" TreeType, match "blob" BlobType, match "commit" CommitType]

permsParser :: Monad m => ParsecT String u m Perms
permsParser = choice [match "120000" SymLinkPerms, match "040000" DirPerms, match "100755" ExecPerms, match "100644" ReadPerms]

data ObjectType
  = TreeType
  | CommitType
  | BlobType
  deriving (Show)

parseObject = try (Tree <$> treeParser) <|> try (CommitObject <$> commitParser) <|> (Blob <$> many1 anyChar)

data Object = Blob String | Tree [TreeEntry] | CommitObject Commit deriving (Show)

data ObjectSummary = ObjectSummary
  { hash :: Reference,
    objectType :: ObjectType,
    byteCount :: Int
  }
  deriving (Show)

type RepoSummary = [ObjectSummary]

-- symlink is 120000
-- dir is 040000
-- executable is 100755
-- readable is 100644
data Perms
  = DirPerms
  | SymLinkPerms
  | ExecPerms
  | ReadPerms
  deriving (Show)

type Reference = String

data TreeEntry = TreeEntry
  { perms :: Perms,
    treeLineType :: ObjectType,
    fileName :: String,
    ref :: Reference
  }
  deriving (Show)

type Tree = [TreeEntry]

-- date is seconds since jan 1 1970. each commit and author has one
-- format is NAME <email> seconds timezone
data UserInfo = UserInfo
  { userName :: String,
    userEmail :: String,
    userDateSeconds :: Integer,
    userTimezone :: String
  }
  deriving (Show)

data Commit = Commit
  { tree :: Reference,
    ancestors :: [Reference],
    author :: UserInfo,
    committer :: UserInfo,
    message :: String
  }
  deriving (Show)

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