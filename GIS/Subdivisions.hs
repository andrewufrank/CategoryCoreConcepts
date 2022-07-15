-----------------------------------------------------------------------------
--
-- Module      :  functions to produce a triangulation and deal with faces 
-- storing in triple store, using a half-quad-edge structure (HQ)
-- approach: use hgeometry to construct the triangulation
--              then use the output to create the triples 
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

module GIS.Subdivisions
 
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



-- import GIS.Category
import GIS.Store
-- import GIS.Store_data
-- import GIS.Functions

-- imports for hgeometry 
import Data.Ext ( type (:+)(..) )

import Data.Geometry.Point
-- import Linear.V2 -- ( V2(..) )
-- import GHC.Generics
-- import qualified Data.List.NonEmpty as NE
-- import Algorithms.Geometry.DelaunayTriangulation.Naive
-- import Algorithms.Geometry.DelaunayTriangulation.Types
--     -- ( Triangulation (_neighbours), toPlanarSubdivision, toPlaneGraph, vertexIds )
-- -- import Data.PlaneGraph ( PlaneGraph )
-- -- import Data.PlaneGraph ( PlaneGraph )
-- import Data.Geometry.PlanarSubdivision
-- import Data.PlaneGraph (boundary)
--     -- ( PlanarSubdivision, PlaneGraph, vertices )
--     -- ( PlanarSubdivision, PlaneGraph, edgeSegment )
-- -- import qualified Data.CircularList as CL
-- import qualified Data.Vector.Circular as Vector
-- -- import qualified Data.Vector as V
-- -- import Data.Aeson.Encode.Pretty (encodePretty)

-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )




outputSubdivisions :: ErrIO () 
outputSubdivisions = do 
    putIOwords ["the tests for subdivisions, primarily triangulation"]

--------- data for test 

twoT = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]
-- test points - x, y, id 

-- tests points used initially
qs = [(Point2  0 0) :+ 'a' , Point2  1.5 1.5 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 0  :+ 'd']