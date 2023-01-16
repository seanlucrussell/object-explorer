{-# LANGUAGE OverloadedStrings #-}

module Renderers (renderSummary, renderObject) where

import System.Posix.Internals (c_access, c_ftruncate, fileType)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (Commit (..), Object (..), ObjectSummary (ObjectSummary, objectType), ObjectType (..), Perms (..), Reference, RepoSummary, TreeEntry (TreeEntry), UserInfo (..))

renderSummary :: RepoSummary -> Html
renderSummary s = docTypeHtml $ do
  H.head $ do
    H.title "overview"
  body $ table $ mapM_ (tr . renderObjectSummary) s

renderObjectSummary :: ObjectSummary -> Html
renderObjectSummary (ObjectSummary ref objectType byteCount) = do
  td $ renderObjectType objectType
  td $ renderReference ref
  td $ toHtml byteCount

x :: Reference
x = "a"

renderObject :: Reference -> Object -> Html
renderObject ref o = docTypeHtml $ do
  H.head $ do
    H.title (toHtml ref)
  body $ do
    h1 (toHtml ("Object " ++ ref))
    case o of
      Blob s -> pre (toHtml s)
      Tree tes -> table $ renderTreeEntries tes
      CommitObject com -> renderCommit com

renderTreeEntries :: [TreeEntry] -> Html
renderTreeEntries = mapM_ (tr . renderTreeEntry)

renderPerms :: Perms -> Html
renderPerms ReadPerms = "100644"
renderPerms ExecPerms = "100755"
renderPerms SymLinkPerms = "120000"
renderPerms DirPerms = "100644"

renderObjectType :: ObjectType -> Html
renderObjectType BlobType = "blob"
renderObjectType TreeType = "tree"
renderObjectType CommitType = "commit"

renderTreeEntry :: TreeEntry -> Html
renderTreeEntry (TreeEntry perms objectType reference filename) = do
  td $ i (renderPerms perms)
  td $ renderObjectType objectType
  td $ renderReference reference
  td $ em (toHtml filename)

renderReference :: Reference -> Html
renderReference r = a ! href (toValue (r ++ ".html")) $ toHtml r

renderCommit :: Commit -> Html
renderCommit (Commit tree parents author committer message) = do
  "Tree: "
  renderReference tree
  "Parents: "
  ul $ mapM_ (li . renderReference) parents
  "Author: "
  renderUser author
  "Committer: "
  renderUser committer
  "Commit message: "
  p (toHtml message)

renderUser :: UserInfo -> Html
renderUser (UserInfo name email date timezone) = do
  toHtml name
  i (toHtml email)
  toHtml date
  toHtml timezone