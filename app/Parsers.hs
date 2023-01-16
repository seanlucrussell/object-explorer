module Parsers (parseObject, repoSummaryParser) where

import Data.Functor (($>))
import qualified Data.Functor.Identity
import Text.Parsec
  ( Parsec,
    anyChar,
    char,
    choice,
    digit,
    endOfLine,
    letter,
    many,
    many1,
    manyTill,
    newline,
    noneOf,
    sepEndBy,
    sepEndBy1,
    space,
    spaces,
    string,
    try,
    (<|>),
  )
import Types
  ( Commit (Commit),
    Object (Blob, CommitObject, Tree),
    ObjectSummary (ObjectSummary),
    ObjectType (..),
    Perms (..),
    TreeEntry (TreeEntry),
    UserInfo (UserInfo),
  )

repoSummaryParser :: Parsec String u [ObjectSummary]
repoSummaryParser = sepEndBy1 objectSummaryParser newline
  where
    objectSummaryParser = do
      hash <- parseHash
      spaces
      objectType <- objectTypeParser
      spaces
      byteCount <- read <$> many1 digit
      return (ObjectSummary hash objectType byteCount)

treeParser :: Parsec String u [TreeEntry]
treeParser = sepEndBy1 treeEntryParser newline
  where
    treeEntryParser = do
      perms <- permsParser
      spaces
      objectType <- objectTypeParser
      spaces
      hash <- parseHash
      spaces
      fileName <- many (noneOf "\n")
      return (TreeEntry perms objectType hash fileName)

commitParser :: Parsec String u Commit
commitParser = do
  string "tree"
  space
  treeHash <- parseHash
  newline
  parents <-
    sepEndBy
      ( do
          string "parent"
          space
          parseHash
      )
      newline
  string "author"
  spaces
  author <- parseUser
  string "committer"
  spaces
  committer <- parseUser
  newline
  message <- many1 anyChar
  return
    ( Commit
        treeHash
        parents
        author
        committer
        message
    )

parseUser :: Parsec String u UserInfo
parseUser = do
  authorName <- manyTill anyChar (char '<')
  authorEmail <- manyTill anyChar (char '>')
  spaces
  authorTime <- read <$> many1 digit
  spaces
  authorTimezone <- manyTill anyChar newline
  return (UserInfo authorName authorEmail authorTime authorTimezone)

parseHash :: Parsec String u [Char]
parseHash = many1 (letter <|> digit)

match :: String -> a -> Parsec String u a
match a b = try (string a $> b)

objectTypeParser :: Parsec String u ObjectType
objectTypeParser = choice [match "tree" TreeType, match "blob" BlobType, match "commit" CommitType]

permsParser :: Parsec String u Perms
permsParser = choice [match "120000" SymLinkPerms, match "040000" DirPerms, match "100755" ExecPerms, match "100644" ReadPerms]

parseObject :: Parsec String u Object
parseObject = try (Tree <$> treeParser) <|> try (CommitObject <$> commitParser) <|> (Blob <$> many1 anyChar)
