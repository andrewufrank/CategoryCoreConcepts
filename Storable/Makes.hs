-----------------------------------------------------------------------------
--
-- Module      :  makeXX are the functions to store sobj 
{-  

 uses the storage to put relations into store!
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

module Storable.Makes
 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    -- ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State

import Vault.Triple4cat
    ( Action(Ins),
      CatStores(catStoreBatch, catStoreEmpty),
      CatStore,
      find2rel,
      find2fun,
      MorphSel(Forward, Inv) )

-- import GIS.Category
-- import Storable.Sobj  -- the storable objects 
-- import Storable.Srel -- the storable relations
import Storable.Store 

------------------------------------------------------------------
-- the makes 
-- produce a list of storeElements 
-- they are colled with just the Int id and put the type around it

makeEdgeStartNode :: Int -> Int -> StoreElementList
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeStartNode o1 o2 = [(EdgeTag (Edge o1), sMorph, NodeTag (Node o2))]

makeEdgeEndNode :: Int -> Int ->   StoreElementList
makeEdgeEndNode o1 o2 = [(EdgeTag (Edge o1), tMorph, NodeTag (Node o2))]

-- point is not storable, is a value
-- store node
-- makePoint :: Node ->  Double -> Double ->   StoreElementList
-- -- add a function name if necessary
-- makePoint i x y = (NodeTag ( i), xyMorph, PointTag (Point2d x y))

makeSCost e c = [(EdgeTag (Edge e), scMorph, CostTag (Cost c))]
-- makeTCost e c = (EdgeTag (Edge e), tcMorph, CostTag (Cost c))
-- cost is on edge, one direction only... 

makePoint :: Int -> Text -> Double -> Double -> [StoreElement]
-- | to create a node with the nodeid and the given name and x y 
makePoint i n x y = makeNode1 i n x y

-- makeNode :: (Show a) => Int -> (Int, (PtTuple a)) ->  [StoreElementList]
-- -- | the first is a offset for the node id
-- -- | same for both nodes and edges 
-- -- | id for edge (s t)

-- the base make node with text name and two doubles 
makeNode1 ::  Int -> Text -> Double -> Double  ->  [StoreElement]
makeNode1  i n x y = 
    [ (NodeTag node, xyMorph, PointTag (Point2d x y))
    , (NodeTag node, nameMorph, NameTag (Name n))]
    where node = Node i
