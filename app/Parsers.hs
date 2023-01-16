module Parsers (parseObject, repoSummaryParser) where

import Data.Functor (($>))
import qualified Data.Functor.Identity
import Text.Parsec
  ( ParsecT,
    anyChar,
    char,
    choice,
    digit,
    letter,
    many1,
    manyTill,
    newline,
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

parseUser :: ParsecT String u Data.Functor.Identity.Identity UserInfo
parseUser = do
  authorName <- manyTill anyChar (char '<')
  authorEmail <- manyTill anyChar (char '>')
  spaces
  authorTime <- read <$> many1 digit
  spaces
  authorTimezone <- manyTill anyChar newline
  return (UserInfo authorName authorEmail authorTime authorTimezone)

parseHash :: ParsecT String u Data.Functor.Identity.Identity [Char]
parseHash = many1 (letter <|> digit)

match :: Monad m => String -> a -> ParsecT String u m a
match a b = try (string a $> b)

objectTypeParser :: Monad m => ParsecT String u m ObjectType
objectTypeParser = choice [match "tree" TreeType, match "blob" BlobType, match "commit" CommitType]

permsParser :: Monad m => ParsecT String u m Perms
permsParser = choice [match "120000" SymLinkPerms, match "040000" DirPerms, match "100755" ExecPerms, match "100644" ReadPerms]

parseObject = try (Tree <$> treeParser) <|> try (CommitObject <$> commitParser) <|> (Blob <$> many1 anyChar)
