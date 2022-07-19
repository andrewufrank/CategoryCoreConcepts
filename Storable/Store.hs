-----------------------------------------------------------------------------
--
-- Module      :  the store for the category in GIS.Category
{-  

 construct the two sum types and the storage

 the s-obj are in the subj (o) and value (v) values in the triples 
 
 the s-morphism are the relations in the triple store 
 (the p = predicate) 
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

module Storable.Store
 
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
import Storable.Sobj  -- the storable objects 

----------- the category


-- | the morphism in the category, required for store
data MorphPoint = Stag S | Ttag T | XYtag XY | DistTag Distance 
        | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        | Nametag
        | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
-- instance MorphPoint Zeros where zero = ZZm

-- constants for the tags (some have an argument, some not)
xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Distance 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T
scMorph = SCosttag SC
-- tcMorph = TCcosttag TC
nameMorph = Nametag 

------------------ the objects 
------ the object sum type  with tags 

-- | the objects in the category - required for store
-- reference to types with no parameter
data ObjPoint = NodeTag Node | EdgeTag Edge | PointTag (Point2) | ValueTag (ValueType Length)  | CostTag Cost 
    | NameTag Name
    | HQTag HQType
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjPoint where zero = ZZpoint

unEdgeTag :: ObjPoint -> Edge 
unEdgeTag (EdgeTag t) = t 
unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

unNodeTag :: ObjPoint -> Node 
unNodeTag (NodeTag t) = t 
unNodeTag x = errorT ["unNodeTag - not a Node", showT x]
unPointTag :: ObjPoint -> Point2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unNodeTag - not a Node", showT x]
unValueTag :: ObjPoint -> ValueType Length
unValueTag (ValueTag t) = t 
unValueTag x = errorT ["unNodeTag - not a Node", showT x]
unCostTag (CostTag t) = t 
unCostTag x = errorT ["unCostTag -  not a Cost", showT x]

-- code depending on MonadState
type Store = CatStore ObjPoint MorphPoint
type StoreStateMonad = State Store  
type StoreErrIO = StateT Store ErrIO


-- the makes for all ...
makeEdgeStartNode :: Int -> NodeID -> (ObjPoint, MorphPoint, ObjPoint)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeStartNode o1 o2 = (EdgeTag (Edge o1), sMorph, NodeTag (Node o2))

makeEdgeEndNode :: Int -> NodeID ->   (ObjPoint, MorphPoint, ObjPoint)
makeEdgeEndNode o1 o2 = (EdgeTag (Edge o1), tMorph, NodeTag (Node o2))


makePoint :: NodeID ->  Double -> Double ->   (ObjPoint, MorphPoint, ObjPoint)
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



