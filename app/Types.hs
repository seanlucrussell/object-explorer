module Types
  ( ObjectType (..),
    ObjectSummary (..),
    Object (..),
    Perms (..),
    Reference,
    TreeEntry (..),
    Tree,
    UserInfo (..),
    Commit (..),
  )
where

data ObjectType
  = TreeType
  | CommitType
  | BlobType
  deriving (Show)

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
