-----------------------------------------------------------------------------
--
-- Module      :  building points with 2 coordinate values
{-  

two objects: Point, Coord  (Point = Coord x Coord)
two morphism: distance :: Point -> Point -> Distance 

Coord and Distance are Float (simplistic)

for computation of distance use package hgeom 

 

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
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib.Point
    -- (ObjST (..)
    -- , MorphST
    -- -- , MorphST'(Left, Right)
    -- -- , Left, Right
    -- , Wobj(..), Bobj(..)
    -- -- , either, distribute
    -- , b', w', h, k
    -- , T(..), S(..)
    -- , makeEdgeSpace, makeEdgeBusiness
    -- -- for test 
    -- , pageST_withTriples
    -- , pageST_withTriplesBusiness
    -- , pageSTproductCombines
    -- )    
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- end 

import UniformBase 
import Control.Monad.State
    ( MonadState(get), evalState, runState, State )

-- import Lib.Rules
-- import Vault.Values
-- import Data.List.Extra
-- import Data.Bifunctor (bimap)
-- import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat
    ( Action(..),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind) )
import Lib.EdgeNodeGraph 
import qualified Graphics.Gloss as Gloss

----------- the category

type MorphPoint = Distance 

data Distance = Distance deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data ObjPoint = PointTag (PointType Text)  -- is ObjST in other 
-- data PointType t = PT Point 2 t deriving (Show, Read, Ord, Eq, Generic, Zeros)

data Point2 = Point2 Gloss.Point -- (Float, Float)  -- the data type from gloss 
-- a point in 2d (simplistic from gloss)
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
    
data Length = Length Float  
-- a distance value, should be a subobj of Value 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data ObjPoint = PointType Point2 | ValueType Length  
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- p1 = HGP.Point2 1 1

--------------- ---------------------example
pagePoint :: ErrIO ()
pagePoint = do
    putIOwords ["\n pagePoint"]