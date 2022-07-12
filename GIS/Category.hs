-----------------------------------------------------------------------------
--
-- Module      :  the objects (cobj) and morphsim (morph) for the 
--                  category GIS
{-  

these are only the top level definitions 
subobjects and submorphs can be done in specific modules 

the storage (with the sum types) are separate

-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GIS.Category
    ( module GIS.Category
    -- , module Lib.Points
    )
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
 
import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
import Vault.Triple4cat () -- for instance Zeros Float

-- import Lib.Points 
-- the type (top level?)

-- import Control.Monad.State
    -- ( MonadState(get), evalState, runState, State, StateT, execStateT )

----------- the category

-- the names for the morphism

data XY = XY 
    deriving (Show, Read, Ord, Eq, Generic)
data Distance = Distance 
    deriving (Show, Read, Ord, Eq, Generic)

data S = S  deriving (Show, Read, Ord, Eq, Generic)
-- | the start node of an edge
data SC = SC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards s (reverse)
data T = T  deriving (Show, Read, Ord, Eq, Generic)
-- | the end node of an edge 
data TC = TC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards t (forward)

------------------ the objects 
---- the xxTypes serve to allow further specifications
data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type Node = NodeType Char 
type Edge = EdgeType Int

-- | the cost to measure the use of a resource 
data Cost = Cost Int 
        deriving (Show, Read, Ord, Eq, Generic, Zeros)

data Point2 = Point2 Float Float  -- the data type from gloss 
-- a point in 2d (simplistic from gloss)
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
    
data Length = Length Float  
-- a distance value, should be a subobj of Value 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data PointType c = PointType Point2 deriving (Show, Read, Ord, Eq, Generic, Zeros)

data ValueType c = Value c deriving (Show, Read, Ord, Eq, Generic, Zeros)


-- --------------------data 

-- graph123 :: [Action (ObjPoint, MorphPoint, ObjPoint)]
-- graph123 = [Ins (makeNodeStartingEdge 'e' 1)
--     , Ins (makeNodeEndingEdge    'f' 1)
--     , Ins (makeNodeStartingEdge   'f' 2)
--     , Ins (makeNodeEndingEdge     'g' 2)
--     , Ins (makePoint 'e' 0 0)
--     , Ins (makePoint 'f' 1 1)
--     ]
-- graphShortestPathEx :: [Action (ObjPoint, MorphPoint, ObjPoint)]
-- graphShortestPathEx = 
--     [ Ins (makeNodeStartingEdge 'a' 1)
--     , Ins (makeNodeEndingEdge 'b' 1)
--     , Ins (makeNodeStartingEdge 'b' 2)
--     , Ins (makeNodeEndingEdge 'c' 2)
--     , Ins (makeNodeStartingEdge 'b' 3)
--     , Ins (makeNodeEndingEdge 'c' 3)
--     , Ins (makeNodeStartingEdge 'c' 4)
--     , Ins (makeNodeEndingEdge 'a' 4)
--     , Ins (makeNodeStartingEdge 'a' 5)
--     , Ins (makeNodeEndingEdge 'c' 5)
--     , Ins (makeSCost 1 1)
--     , Ins (makeSCost 2 2)
--     , Ins (makeSCost 3 5)
--     , Ins (makeSCost 4 1)
--     , Ins (makeSCost 5 5)
--     ]

-- cat0 :: CatStore ObjPoint MorphPoint
-- cat0 = catStoreEmpty
-- cat2 :: CatStore ObjPoint MorphPoint
-- cat2 = catStoreBatch  graph123
--      cat0
-- cat11 = catStoreBatch graphShortestPathEx cat0
-- --------------- ---------------------example
-- pagePoint :: ErrIO ()
-- pagePoint = do
--     putIOwords ["\n pagePoint"]
--     -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
--     let p1 = evalState (xyFun (Node 'e')) cat2
--     putIOwords ["the point for node e", showT p1]
--     let p1f = evalState (xyFun (Node 'f')) cat2
--     putIOwords ["the point for node f", showT p1f]
--     -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
--     -- putIOwords ["the distance 1 t 2", showT d1]
--     -- let le = evalState (lengthEdge (Edge 1)) cat2
--     -- putIOwords ["the length of the edge 1", showT le]

--     putIOwords ["cat11", showT cat11]
--     newcat11 <- execStateT (runWithState) cat11
--     putIOwords ["newcat11", showT newcat11]

--     -- let nc = evalState (costOutgoingEdges (Node 'a')) cat2
--     -- putIOwords ["the node-cost pairs at Node a", showT nc]


-- f op = evalState op cat11

-- runWithState :: StoreErrIO Store
-- runWithState = do 
--     putIOwords ["runWithState"]
--     let le = evalState (lengthEdge (Edge 1)) cat11
--     putIOwords ["the length of the edge 1", showT le]

--     let nc = evalState (costOutgoingEdges (Node 'a')) cat11
--     putIOwords ["the node-cost pairs at Node a", showT nc]


--     s <- get 
--     return s

