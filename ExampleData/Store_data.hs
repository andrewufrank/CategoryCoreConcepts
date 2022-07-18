-----------------------------------------------------------------------------
--
-- Module      :  the store for the category in GIS.Category
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
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ExampleData.Store_data
 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State

import Vault.Triple4cat
 

import GIS.Category
import GIS.Store  
import GIS.Functions
import GIS.FunGeometry
import GIS.Subdivisions

--------------------data 

graph123 :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graph123 = [Ins (makeEdgeStartNode 1 "e" )
    , Ins (makeEdgeEndNode   1 "f" )
    , Ins (makeEdgeStartNode  2 "f" )
    , Ins (makeEdgeEndNode   2  "g" )
    , Ins (makePoint "e" 0 0)
    , Ins (makePoint "f" 1 1)
    ]
graphShortestPathEx :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graphShortestPathEx = 
    [ Ins (makeEdgeStartNode 1 "a" )
    , Ins (makeEdgeEndNode 1 "b" )
    , Ins (makeEdgeStartNode 2 "b" )
    , Ins (makeEdgeEndNode 2 "c" )
    , Ins (makeEdgeStartNode 3 "c" )
    , Ins (makeEdgeEndNode 3 "b" )
    , Ins (makeEdgeStartNode  4 "c" )
    , Ins (makeEdgeEndNode 4 "a" )
    , Ins (makeEdgeStartNode 5 "a" )
    , Ins (makeEdgeEndNode 5 "c" )
    , Ins (makeSCost 1 1)
    , Ins (makeSCost 2 2)
    , Ins (makeSCost 3 5)
    , Ins (makeSCost 4 1)
    , Ins (makeSCost 5 5)
    , Ins (makePoint "a" 0 0)
    , Ins (makePoint "b" 1 1)
    ]

cat0 :: CatStore ObjPoint MorphPoint
cat0 = catStoreEmpty
cat2 :: CatStore ObjPoint MorphPoint
cat2 = catStoreBatch  graph123
     cat0
cat11 = catStoreBatch graphShortestPathEx cat0

f op = evalState op cat11

--------------- ---------------------example
pageStore :: ErrIO ()
pageStore = do
    putIOwords ["\n ------------------- pageStore"]
    -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
    let p1 = evalState (xyFun (Node "e")) cat2
    putIOwords ["the point for node e", showT p1]
    let p1f = evalState (xyFun (Node "f")) cat2
    putIOwords ["the point for node f", showT p1f]
    -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
    -- putIOwords ["the distance 1 t 2", showT d1]
    -- let le = evalState (lengthEdge (Edge 1)) cat2
    -- putIOwords ["the length of the edge 1", showT le]


    let n1 = evalState (sInvRel (Node "a")) cat11 -- > [Edge 1,Edge 5]
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
    n1 <- sInvRel (Node "a") -- > [Edge 1,Edge 5]
    putIOwords ["sRel von Node a", showT n1]
    e1 <- sFun (Edge 1)  -- > (Node a)
    putIOwords ["sInv from Edge 1", showT e1]
    p1 <- xyFun (Node "a")
    putIOwords ["the xy of Edge 1", showT p1]
    le <- lengthEdge (Edge 1) 
    putIOwords ["the length of the edge 1", showT le]

    let nc = evalState (costOutgoingEdges (Node "a")) cat11
    putIOwords ["the node-cost pairs at Node a", showT nc]


    -- s <- get 
    return ()

-- to test shortest path function 

main1 = print $ dijkstra step "c" (0 , "a")
    where
        step :: (Int , NodeID) -> [(Int , NodeID)]
        step (cost1 , node1) =
            [ (cost1 + edgeCost , child)
            | (Node child, Cost edgeCost ) <- evalState (costOutgoingEdges (Node node1))cat11
                    ]
-- replace Map.lookup node with costOutgoingEdges
-- evalState (costOutgoingEdges node) cat11
-- removed fromMaybe []  (because find returns [])
-- and flip result

main2 :: IO ()
main2 =  do  -- with tests in other modules
    main1
    print $  (shortestPathCostOnly cat11 (0, "a") "c") 

    print $  (shortestPathWithPath cat11 (PathChar 0 ["a"], "a") "c") 
    -- dirMain
    -- openMain
    let resA = shortA cat11 (PathChar 0 ["a"], "a") "c"
    putIOwords ["the resA", showT resA]
    return ()

offset_two = 200 :: Int

posTriple_two = map (makeNode offset_two) pos_two
edgeTriple_two = map (makeHQ offset_two)    $ edge_two
main3 :: IO ()
main3 =  do  -- with tests in other modules
    putIOwords ["loading the data from triangulation two"] 
    putIOwords ["the node data", showT pos_two]
    putIOwords ["the edge data", showT edge_two]
    putIOwords ["the nodeTriple", showT posTriple_two]
    putIOwords ["the edgeTriple", showT edgeTriple_two]
    return ()

wrapIns' a =   Ins  a

catTwo0 = catStoreEmpty
catTwo1 = catStoreBatch (map wrapIns' . concat $ posTriple_two) catTwo0
catTwo2 = catStoreBatch (map wrapIns' . concat $ edgeTriple_two) catTwo1


{- 
loading the data from triangulation two
code tri_two, pos_two, edge_two 
then posTriple_two, edgeTriple_two
the node data [(0,(0.0,0.0,"11")),(1,(1.5,1.5,"12")),(2,(0.0,2.0,"13")),(3,(2.0,0.0,"14"))]
the edge data [(0,2),(0,1),(0,3),(1,3),(1,0),(1,2),(2,1),(2,0),(3,0),(3,1)]
the nodeTriple [[(NodeTag (Node "200"),XYtag XY,PointTag (Point2 0.0 0.0)),(NodeTag (Node "200"),Nametag,NameTag (Name "\"11\""))],[(NodeTag (Node "201"),XYtag XY,PointTag (Point2 1.5 1.5)),(NodeTag (Node "201"),Nametag,NameTag (Name "\"12\""))],[(NodeTag (Node "202"),XYtag XY,PointTag (Point2 0.0 2.0)),(NodeTag (Node "202"),Nametag,NameTag (Name "\"13\""))],[(NodeTag (Node "203"),XYtag XY,PointTag (Point2 2.0 0.0)),(NodeTag (Node "203"),Nametag,NameTag (Name "\"14\""))]]
the edgeTriple [[(HQTag (HQ 200 202),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 202),Stag S,NodeTag (Node "202"))],[(HQTag (HQ 200 201),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 201),Stag S,NodeTag (Node "201"))],[(HQTag (HQ 200 203),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 203),Stag S,NodeTag (Node "203"))],[(HQTag (HQ 201 203),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 203),Stag S,NodeTag (Node "203"))],[(HQTag (HQ 201 200),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 201 202),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 202),Stag S,NodeTag (Node "202"))],[(HQTag (HQ 202 201),Stag S,NodeTag (Node "202")),(HQTag (HQ 202 201),Stag S,NodeTag (Node "201"))],[(HQTag (HQ 202 200),Stag S,NodeTag (Node "202")),(HQTag (HQ 202 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 203 200),Stag S,NodeTag (Node "203")),(HQTag (HQ 203 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 203 201),Stag S,NodeTag (Node "203")),(HQTag (HQ 203 201),Stag S,NodeTag (Node "201"))]]
-}