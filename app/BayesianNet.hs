module BayesianNet where

import Data.HashMap.Strict (HashMap, insert, member)

data Node = Node {name::String, parent:: [String], numValues:: Int, table:: HashMap ([Int], Int) Double} deriving (Show)

newtype BayesianNet = BayesianNet {nodeMap:: HashMap String Node} deriving (Show)

addNode::BayesianNet -> Node -> BayesianNet
addNode net node  
    | member (name node) (nodeMap net)  = BayesianNet (nodeMap net)
    | otherwise                         = BayesianNet (insert (name node) node (nodeMap net))

solving::BayesianNet -> HashMap String Int -> HashMap String Int -> Double
solving net probNodes givenNodes    = 