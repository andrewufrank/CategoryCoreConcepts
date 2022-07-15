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
-- for hgeometry
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )

-- imports for hgeometry 
import Data.Ext ( type (:+)(..) )

import Data.Geometry.Point
-- import Linear.V2 -- ( V2(..) )
-- import GHC.Generics
import qualified Data.List.NonEmpty as NE
import Algorithms.Geometry.DelaunayTriangulation.Naive
import Algorithms.Geometry.DelaunayTriangulation.Types
--     -- ( Triangulation (_neighbours), toPlanarSubdivision, toPlaneGraph, vertexIds )
-- -- import Data.PlaneGraph ( PlaneGraph )
-- -- import Data.PlaneGraph ( PlaneGraph )
-- import Data.Geometry.PlanarSubdivision
-- import Data.PlaneGraph (boundary)
--     -- ( PlanarSubdivision, PlaneGraph, vertices )
--     -- ( PlanarSubdivision, PlaneGraph, edgeSegment )
import qualified Data.CircularList as CL
-- import qualified Data.Vector.Circular as Vector
import qualified Data.Vector as V
-- -- import Data.Aeson.Encode.Pretty (encodePretty)






outputSubdivisions :: ErrIO () 
outputSubdivisions = do 
    putIOwords ["the tests for subdivisions, primarily triangulation"]

--------- data for test 
type PtTuple = (Double, Double, Int)
-- toPoint2 :: [PtTuple] -> [Point 2 Double :+ Int]
toPoint2   = map (\(x,y,i) -> (Point2 x y :+ i)) 

twoT :: [PtTuple]
twoT = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]
-- test points - x, y, id 

tri_two = toTri twoT --  delaunayTriangulation . NE.fromList . toPoint2 $ twoT

toTri = delaunayTriangulation . NE.fromList . toPoint2

qtwo = toPoint2 twoT
threeT = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]
-- tests points used initially
qs = [(Point2  0 0) :+ 'a' , Point2  1.5 1.5 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 0  :+ 'd']

t1 :: Triangulation Char Float
t1 = delaunayTriangulation . NE.fromList $ qs 
-- verts1 :: Vector (CList VertexID)
verts1 = _neighbours t1
-- pos1 = edgesAsPoints t1 -- gives all possible pairs
-- po11 :: [(Point 2 Float :+ Char)]
-- pos1 ::  Vector (Point 2 Float :+ Char)
pos1 =  _positions t1
-- pos1l :: [a]
-- pos1l =  fromJust . Vector.fromList .  CL.fromList $ pos1
pos1l =  V.toList pos1
unPoint2 (Point2 x y :+ c) = (x, y, c)
-- unLoc (p1,p2) = (unPoint2 p1, unPoint2 p2)
pos1m :: [(Float, Float, Char)]
pos1m = map unPoint2  pos1l
pos1mx = zip [1..] pos1m  
-- ready to convert to node triples with the local index first arg
-- then x,y, and the name given in the input for the triangulation 
pos_two = toPos tri_two 
    -- zip [1..] . V.toList . _positions . delaunayTriangulation . NE.fromList . toPoint2 $ twoT

toPos= zip [1..] . map unPoint2 . V.toList . _positions

verts1l :: [CL.CList VertexID]
verts1l = V.toList verts1 
verts1ll :: [[Int]]
-- verts1ll :: [[VertexID]]
verts1ll = map CL.toList  verts1l   -- VertexID is Int
ver1mx :: [(Int, [Int])]
ver1mx = zip [1..] verts1ll
edgesPerNode :: (a, [b]) -> [(a, b)]
edgesPerNode (s,[]) = []
edgesPerNode (s,t:ts) = (s,t): edgesPerNode (s,ts)
-- this gives all the half-quad-edges, i.e. 
-- one for (hq id, start, s), (hq id+, end, t), etc. 
-- the faces must be reconstructed from following the halfquads 
-- around a face 
edgePairs = map edgesPerNode ver1mx

toEdge = map edgesPerNode . zip [1..]. map CL.toList . V.toList . _neighbours 
edge_two = toEdge tri_two 