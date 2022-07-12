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

module GIS.Store_data
 
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

--------------------data 

graph123 :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graph123 = [Ins (makeNodeStartingEdge 'e' 1)
    , Ins (makeNodeEndingEdge    'f' 1)
    , Ins (makeNodeStartingEdge   'f' 2)
    , Ins (makeNodeEndingEdge     'g' 2)
    , Ins (makePoint 'e' 0 0)
    , Ins (makePoint 'f' 1 1)
    ]
graphShortestPathEx :: [Action (ObjPoint, MorphPoint, ObjPoint)]
graphShortestPathEx = 
    [ Ins (makeNodeStartingEdge 'a' 1)
    , Ins (makeNodeEndingEdge 'b' 1)
    , Ins (makeNodeStartingEdge 'b' 2)
    , Ins (makeNodeEndingEdge 'c' 2)
    , Ins (makeNodeStartingEdge 'c' 3)
    , Ins (makeNodeEndingEdge 'b' 3)
    , Ins (makeNodeStartingEdge 'c' 4)
    , Ins (makeNodeEndingEdge 'a' 4)
    , Ins (makeNodeStartingEdge 'a' 5)
    , Ins (makeNodeEndingEdge 'c' 5)
    , Ins (makeSCost 1 1)
    , Ins (makeSCost 2 2)
    , Ins (makeSCost 3 5)
    , Ins (makeSCost 4 1)
    , Ins (makeSCost 5 5)
    , Ins (makePoint 'a' 0 0)
    , Ins (makePoint 'b' 1 1)
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
    let p1 = evalState (xyFun (Node 'e')) cat2
    putIOwords ["the point for node e", showT p1]
    let p1f = evalState (xyFun (Node 'f')) cat2
    putIOwords ["the point for node f", showT p1f]
    -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
    -- putIOwords ["the distance 1 t 2", showT d1]
    -- let le = evalState (lengthEdge (Edge 1)) cat2
    -- putIOwords ["the length of the edge 1", showT le]


    let n1 = evalState (sRel (Node 'a')) cat11 -- > [Edge 1,Edge 5]
    putIOwords ["sRel von Node a", showT n1]

    putIOwords ["cat11", showT cat11]
    newcat11 <- runStateT (runWithState) cat11
    -- putIOwords ["newcat11", showT newcat11]

    -- let nc = evalState (costOutgoingEdges (Node 'a')) cat2
    -- putIOwords ["the node-cost pairs at Node a", showT nc]
    return ()



runWithState :: StoreErrIO ()
runWithState = do 
    putIOwords ["runWithState"]
    -- catxx <- get 
    -- putIOwords ["cat", showT catxx]
    -- let n1 = evalState (sRel (Node 'a')) cat11 -- > [Edge 1,Edge 5]
    -- putIOwords ["sRel von Node a", showT n1]
    n1 <- sRel (Node 'a') -- > [Edge 1,Edge 5]
    putIOwords ["sRel von Node a", showT n1]
    e1 <- sInv (Edge 1)  -- > (Node a)
    putIOwords ["sInv from Edge 1", showT e1]
    p1 <- xyFun (Node 'a')
    putIOwords ["the xy of Edge 1", showT p1]
    le <- lengthEdge (Edge 1) 
    putIOwords ["the length of the edge 1", showT le]

    let nc = evalState (costOutgoingEdges (Node 'a')) cat11
    putIOwords ["the node-cost pairs at Node a", showT nc]


    -- s <- get 
    return ()

