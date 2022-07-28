-----------------------------------------------------------------------------
--
-- Module      :  the objects   which are storable

-- create a sum type for CatStore 
{-  

these are only the top level definitions 
and depend only on subtypes (not yet)
subobjects   can be done in specific modules 

the storage functions are in XXX 
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

module Storable.Sobj
    ( module Storable.Sobj
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
-- import Vault.Triple4cat () -- for instance Zeros Double

-- import Lib.Points 
-- the type (top level?)

-- import Control.Monad.State
    -- ( MonadState(get), evalState, runState, State, StateT, execStateT )

------------------ the objects 
------ the object sum type  with tags 

-- | required for store
--  
data ObjPoint = NodeTag Node | EdgeTag Edge | FaceTag Face
    | HQTag HqType
    | PointTag (Point2d) 
    | LengthTag   Length  | CostTag Cost 
    | NameTag Name
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjPoint where zero = ZZpoint

unEdgeTag :: ObjPoint -> Edge 
unEdgeTag (EdgeTag t) = t 
unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

unNodeTag :: ObjPoint -> Node 
unNodeTag (NodeTag t) = t 
unNodeTag x = errorT ["unNodeTag - not a Node", showT x]

unFaceTag :: ObjPoint -> Face 
unFaceTag (FaceTag t) = t 
unFaceTag x = errorT ["unFaceTag - not a Face", showT x]
unPointTag :: ObjPoint -> Point2d
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unNodeTag - not a Node", showT x]
unCostTag :: ObjPoint -> Cost
unCostTag (CostTag t) = t 
unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
unLengthTag :: ObjPoint -> Length
unLengthTag (LengthTag t) = t 
unLengthTag x = errorT ["unLengthTag - not a Length", showT x]
unNameTag :: ObjPoint -> Name
unNameTag (NameTag t) = t 
unNameTag x = errorT ["unNameTag - not a Name", showT x]


------------------ the storable objects 
-- construct the sum type for objects and value
--
-- the objects are identifier and should all have Int 
-- the data are in the relations, stored in values 

---- the xxTypes serve for further specifications (later perhaps)

data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)

data HqType = Hq Int  
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- the obj for the half-quad edge, derived by the two node ids from hgeometry, but just a simple int as id

data FaceType = Face Int  
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- the id for the half-quad edge, derived by the two node ids from hgeometry, but just a simple int as id


-- type NodeID = Text

type Node = NodeType Int
type Edge = EdgeType Int
type Face = FaceType 
type Hq = HqType 

--------------------------- Value 


-- | the cost to measure the use of a resource 
data Cost = Cost Int 
        deriving (Show, Read, Ord, Eq, Generic, Zeros)

data Point2 = Point2 Double Double  -- the data type from gloss 
-- to replace the name by Point2d 
-- a point in 2d (simplistic from gloss)
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data Point2d = Point2d Double Double 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- | to replace Point2, the data type to represent all 2 coordinate pairs 

data Length = Length Double  
-- a distance value, should be a subobj of Value 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data PointType c = PointType Point2 deriving (Show, Read, Ord, Eq, Generic, Zeros)

data ValueType c = Value c deriving (Show, Read, Ord, Eq, Generic, Zeros)


data Name  = Name Text 
-- names for the points in geometry
-- perhaps better all text?
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance Zeros Double where 
        zero = 0.0 