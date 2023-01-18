{-# LANGUAGE OverloadedStrings #-}

module Renderers (renderSummary, renderObject) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (Commit (..), Object (..), ObjectSummary (ObjectSummary, objectType), ObjectType (..), Perms (..), Reference, RepoSummary, TreeEntry (TreeEntry), UserInfo (..))

basicPage :: String -> Html -> Html
basicPage title content = docTypeHtml $ do
  H.head $ do
    H.title (toHtml title)
    link ! rel "stylesheet" ! href "style.css"
  body $
    main $ do
      content

renderSummary :: RepoSummary -> Html
renderSummary s = basicPage "overview" $ do
  h2 "Repo overview"
  table $ do
    tr $ do
      th "Object type"
      th "Object id"
      th "Byte count"
    mapM_ (tr . renderObjectSummary) s

renderObjectSummary :: ObjectSummary -> Html
renderObjectSummary (ObjectSummary ref objectType byteCount) = do
  td $ renderObjectType objectType
  td $ renderReference ref
  td $ toHtml byteCount

renderObject :: Reference -> Object -> Html
renderObject ref o = basicPage ref $ do
  h6 $ a "Back to overview" ! href "overview.html"
  case o of
    Blob s -> h3 (toHtml ("Blob object " ++ ref)) >> pre (toHtml s)
    Tree tes ->
      h3 (toHtml ("Tree object " ++ ref))
        >> table
          ( do
              tr $ do
                th "Permissions"
                th "Object type"
                th "Object id"
                th "File name"
              renderTreeEntries tes
          )
    CommitObject com -> h3 (toHtml ("Commit object " ++ ref)) >> renderCommit com

renderTreeEntries :: [TreeEntry] -> Html
renderTreeEntries = mapM_ (tr . renderTreeEntry)

renderPerms :: Perms -> Html
renderPerms ReadPerms = "100644"
renderPerms ExecPerms = "100755"
renderPerms SymLinkPerms = "120000"
renderPerms DirPerms = "040000"

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
  p $ i "Tree: " >> renderReference tree
  mapM_ (\parent -> p $ i "Parent: " >> renderReference parent) parents
  p $ i "Author: " >> renderUser author
  p $ i "Committer: " >> renderUser committer
  p $ i "Commit message: " >> toHtml message

renderUser :: UserInfo -> Html
renderUser (UserInfo name email date timezone) =
  b (toHtml name)
    >> ", "
    >> i (toHtml email)
    >> ", "
    >> toHtml date
    >> ", "
    >> toHtml timezone