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
    ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
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
-- the makes for all ...
makeEdgeStartNode :: Int -> Node -> (ObjPoint, MorphPoint, ObjPoint)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeStartNode o1 o2 = (EdgeTag (Edge o1), sMorph, NodeTag (Node o2))

makeEdgeEndNode :: Int -> Node ->   (ObjPoint, MorphPoint, ObjPoint)
makeEdgeEndNode o1 o2 = (EdgeTag (Edge o1), tMorph, NodeTag (Node o2))


makePoint :: Node ->  Double -> Double ->   (ObjPoint, MorphPoint, ObjPoint)
-- add a function name if necessary
makePoint i x y = (NodeTag (Node i), xyMorph, PointTag (Point2 x y))
makeSCost e c = (EdgeTag (Edge e), scMorph, CostTag (Cost c))
-- makeTCost e c = (EdgeTag (Edge e), tcMorph, CostTag (Cost c))

-- the functions and relatiosn to acess the store 

xyFun :: (MonadState (Store) m) =>  Node -> m  (Point2)
-- ^ start with node get point (x y coordinates)
xyFun i = find2fun Forward (NodeTag i) xyMorph unPointTag

sFun :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with node get edge
sFun i = find2fun Forward (EdgeTag i) sMorph unNodeTag 

-- sRel :: (MonadState (Store) m) => Edge -> m [Edge]
-- sRel i = find2rel Forward (EdgeTag i) sMorph unEdgeTag 

tFun :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with node get edge using t
tFun i = find2fun Forward (EdgeTag i) tMorph unNodeTag 

-- tRel :: (MonadState (Store) m) =>  Edge -> m  [Edge]
-- -- ^ start with node get edge using t
-- tRel i = find2rel Forward (NodeTag i) tMorph unEdgeTag 

-- sInv :: (MonadState (Store) m) =>  Edge -> m  Edge
-- -- ^ start with edge get node using s
-- sInv i = find2fun Inv (EdgeTag i) sMorph unNodeTag 

sInvRel :: MonadState (Store) m =>  Node -> m [Edge]
sInvRel i = find2rel Inv (NodeTag i) sMorph unEdgeTag

-- tInv :: (MonadState (Store) m) =>  Edge -> m  Edge
-- -- ^ start with edge get node using t 
-- tInv i = find2fun Inv (EdgeTag i) tMorph unNodeTag

tInvRel :: (MonadState (Store) m) =>  Node -> m  [Edge]
-- ^ start with edge get node using t 
tInvRel i = find2rel Inv (NodeTag i) tMorph unEdgeTag

sCostFun :: MonadState (Store) m => Edge -> m Cost
sCostFun i = find2fun Forward (EdgeTag i) scMorph unCostTag

