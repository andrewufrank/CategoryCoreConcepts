-----------------------------------------------------------------------------
--
-- Module      :  the data for the shortest path examples
{-  

 only the data and the output
-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant return" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module ExampleData.ShortestPath
 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
--     ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State

import Vault.Triple4cat
 

-- import GIS.Category
import Storable.Store  
-- import GIS.Functions
import GIS.FunGeometry
-- import GIS.Triangulation
import Storable.Makes
import Storable.Retrieve

--------------------data 

graph123 :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graph123 = map wrapIns' $ concat [ (makeEdgeStartNode 1 1 )
    ,  (makeEdgeEndNode   1 2 )
    ,  (makeEdgeStartNode  2 2 )
    ,  (makeEdgeEndNode   2  3 )
    ,  (makePoint 1 "e" 0 0)
    ,  (makePoint 2 "f" 1 1)
    ]
graphShortestPathEx :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graphShortestPathEx = map wrapIns' $ concat
    [  (makeEdgeStartNode 1 1 )
    ,  (makeEdgeEndNode 1 2 )
    ,  (makeEdgeStartNode 2 2 )
    ,  (makeEdgeEndNode 2 3 )
    ,  (makeEdgeStartNode 3 3 )
    ,  (makeEdgeEndNode 3 2 )
    ,  (makeEdgeStartNode  4 3 )
    ,  (makeEdgeEndNode 4 1 )
    ,  (makeEdgeStartNode 5 1 )
    ,  (makeEdgeEndNode 5 3 )
    ,  (makeSCost 1 1)
    ,  (makeSCost 2 2)
    ,  (makeSCost 3 5)
    ,  (makeSCost 4 1)
    ,  (makeSCost 5 5)
    ,  (makePoint 1 "a" 0 0)
    ,  (makePoint 2 "b" 1 1)
    ]

cat0 :: CatStore ObjPoint MorphPoint
cat0 = catStoreEmpty
cat2 :: CatStore ObjPoint MorphPoint
cat2 = catStoreBatch  graph123
     cat0
cat11 = catStoreBatch graphShortestPathEx cat0

f op = evalState op cat11

--------------- ---------------------example
-- these output producing functions do not work in repl!
-- build the individual tests - they can be run in repl

pageStore :: ErrIO ()
pageStore = do
    putIOwords ["\n ------------------- pageStore"]
    -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
    let p1 = evalState (xyFun (Node 1)) cat2
    putIOwords ["the point for node e", showT p1]
    let p1f = evalState (xyFun (Node 2)) cat2
    putIOwords ["the point for node f", showT p1f]
    -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
    -- putIOwords ["the distance 1 t 2", showT d1]
    -- let le = evalState (lengthEdge (Edge 1)) cat2
    -- putIOwords ["the length of the edge 1", showT le]


    let n1 = evalState (sInvRel (Node 1)) cat11 -- > [Edge 1,Edge 5]
    putIOwords ["sRel von Node a", showT n1]

    putIOwords ["cat11", showT cat11]
    newcat11 <- runStateT (runWithState) cat11
    -- putIOwords ["newcat11", showT newcat11]

    -- let nc = evalState (costOutgoingEdges (Node "a")) cat2
    -- putIOwords ["the node-cost pairs at Node a", showT nc]
    return ()



runWithState :: StoreErrIO ()
runWithState = do 
    putIOwords ["runWithState"]
    -- catxx <- get 
    -- putIOwords ["cat", showT catxx]
    -- let n1 = evalState (sRel (Node "a")) cat11 -- > [Edge 1,Edge 5]
    -- putIOwords ["sRel von Node a", showT n1]
    n1 <- sInvRel (Node 1) -- > [Edge 1,Edge 5]
    putIOwords ["sRel von Node a", showT n1]
    e1 <- sFun (Edge 1)  -- > (Node a)
    putIOwords ["sInv from Edge 1", showT e1]
    p1 <- xyFun (Node 1)
    putIOwords ["the xy of Edge 1", showT p1]
    le <- lengthEdge (Edge 1) 
    putIOwords ["the length of the edge 1", showT le]

    let nc = evalState (costOutgoingEdges (Node 1)) cat11
    putIOwords ["the node-cost pairs at Node a", showT nc]


    -- s <- get 
    return ()

-- to test shortest path function 

-- main1 = print $ dijkstra step "c" (0 , "a")
--     where
--         step :: (Int , Node) -> [(Int , Node)]
--         step (cost1 , node1) =
--             [ (cost1 + edgeCost , child)
--             | (Node child, Cost edgeCost ) <- evalState (costOutgoingEdges (Node node1))cat11
--                     ]
-- -- replace Map.lookup node with costOutgoingEdges
-- -- evalState (costOutgoingEdges node) cat11
-- -- removed fromMaybe []  (because find returns [])
-- -- and flip result
