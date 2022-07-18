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
import Vault.Triple4cat () -- for instance Zeros Double

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

data Name  = Name Text 
-- names for the points in geometry
-- perhaps better all text?
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

------------------ the storable objects 
---- the xxTypes serve to allow further specifications

data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

data HQType = HQ Int -- Int
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- the id for the half-quad edge, derived by the two node ids from hgeometry, but just a simple int as id
type NodeID = Text

type Node = NodeType NodeID 
type Edge = EdgeType Int

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



