-----------------------------------------------------------------------------
--
-- Module      :  the relations   which are storable

-- create a sum type for CatStore 
{-  
these are only the top level definitions 
and depend only on subtypes (not yet)
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

module Storable.Srel
    ( module Storable.Srel
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

-------------- the names for the storable relations

-- | the sum type for the relation names
data MorphPoint = Stag S | Ttag T | TwinTag Twin| XYtag XY | DistTag Distant
    | CenterTag Center | SurfacedTag Surfaced 
    | HqNodeTag HqNode | HqFaceTag HqFace 
        | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        | NamedTag
        | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
-- instance MorphPoint Zeros where zero = ZZm

-- constants for the tags (some have an argument, some not)
xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Distant 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T
twinMorph = TwinTag Twin
scMorph = SCosttag SC
-- tcMorph = TCcosttag TC
namedMorph = NamedTag 
centerMorph = CenterTag 
surfacedMorph = SurfacedTag 
hqNodeMorph = HqNodeTag
hqFaceMorph = HqFaceTag 

data XY = XY 
    deriving (Show, Read, Ord, Eq, Generic)
data Distant = Distant
    deriving (Show, Read, Ord, Eq, Generic)
data Named = Named 
    deriving (Show, Read, Ord, Eq, Generic)
data Center = Center 
    deriving (Show, Read, Ord, Eq, Generic)
data Surfaced = Surfaced 
    deriving (Show, Read, Ord, Eq, Generic)
data HqNode = HqNode 
    deriving (Show, Read, Ord, Eq, Generic)
data HqFace = HqFace 
    deriving (Show, Read, Ord, Eq, Generic)

data S = S  deriving (Show, Read, Ord, Eq, Generic)
-- | the start node of an edge
data SC = SC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards s (reverse)
data T = T  deriving (Show, Read, Ord, Eq, Generic)
-- | the end node of an edge 
data Twin = Twin deriving (Show, Read, Ord, Eq, Generic)
-- | the twin HQ
data TC = TC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards t (forward)




