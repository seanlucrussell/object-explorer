{-# LANGUAGE OverloadedStrings #-}

module Graph (genGraph) where

import Data.Bits (Bits (xor))
import Data.Graph.Inductive (Gr, LEdge, mkGraph)
import Data.Graph.Inductive.Graph (LNode)
import Data.GraphViz (GraphvizParams, graphToDot, nonClusteredParams)
import Data.GraphViz.Printing (renderDot, toDot)
import Data.GraphViz.Types.Canonical
import Data.Text.Lazy (Text, empty, pack, unpack)
import Types

-- link everything together, color code objects by type. include head and branches
-- object text should include
--   commit message for commits
--   first few lines of blob for blobs
--   nothing for trees
-- arrows should have labels when trees are pointing to other objects

identifier :: Reference -> Int
identifier = foldl (\h c -> 33 * h `xor` fromEnum c) 5381

node :: String -> String -> String -> String
node identifier name color = "    \"" ++ identifier ++ "\" [color=darkgray label=\"" ++ name ++ "\" shape=circle fontname=\"Courier\" style=filled fillcolor=" ++ color ++ "];\n"

edge :: String -> String -> String -> String
edge from to label = "    \"" ++ from ++ "\" -> \"" ++ to ++ "\" [color=darkgray fontsize=10];\n"

shorten :: String -> String
shorten s = take 5 s ++ "\n" ++ take 5 (drop 5 s) ++ "\n" ++ take 2 (drop 10 s) ++ "..."

objectNode :: (Reference, Object) -> String
objectNode (r, Blob s) = node r (shorten r) "white"
objectNode (r, Tree children) = node r (shorten r) "green"
objectNode (r, CommitObject Commit {message = message}) = node r (shorten r) "turquoise"

objectEdges :: (Reference, Object) -> [String]
objectEdges (r, Blob s) = []
objectEdges (r, Tree children) = fmap visualizeChild children
  where
    visualizeChild TreeEntry {fileName = ref, ref = fileName} = edge r ref fileName
objectEdges
  ( r,
    CommitObject
      Commit
        { tree = tree,
          ancestors = parents
        }
    ) = edge r tree "Tree" : fmap refToEdge parents
    where
      refToEdge ref = edge r ref "Parent"

visualize :: RepoSummary -> [(Reference, Object)] -> String
visualize RepoSummary {Types.head = head, branches = branches} objects =
  "digraph repo {\n    graph [overlap=false outputorder=edgesfirst];\n    bgcolor=\"transparent\";\n"
    ++ concat (node "HEAD" "HEAD" "coral" : fmap branchNode (zip [2 ..] branches) ++ fmap objectNode objects)
    ++ concat (edge "HEAD" head "" : fmap branchEdge (zip [2 ..] branches) ++ concatMap objectEdges objects)
    ++ "}"
  where
    branchNode (n, Branch {name = name}) = node name name "yellow"
    branchEdge (n, Branch {name = name, reference = ref}) = edge name ref ""

myParams :: GraphvizParams n Text Text () Text
myParams = nonClusteredParams

genGraph :: RepoSummary -> [(Reference, Object)] -> String -> IO ()
genGraph r o filename = do
  let dot = visualize r o
  writeFile filename dot