-----------------------------------------------------------------------------
--
-- Module      :  functions to produce a triangulation and deal with faces 
-- this is only the stuff which requires/uses hgeometry
-- pass the resulting data to FunGeometry (to avoid clashes with symbols)

-- storing in triple store, using a half-quad-edge structure (HQ)
-- approach: use hgeometry to construct the triangulation
--              then use the output to create the triples 

-- interesting potential for hq id: the azimuth (made unique), the two nodeids (must be unique for a triangulation, with an operation to compute the other one?)

-- needs to construct the dual (i.e. the orbits around the faces - hgeometry?)
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

module GIS.Triangulation
 
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



-- import GIS.Category
-- import GIS.Store
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

-- uses the id produced with hgeometry 
-- will need a preset to have a different id each map

-- toTri from a minimal input data list to a delaunay triangulation
toTri = delaunayTriangulation . NE.fromList . toPoint2

-- the minimal triples for the edges, relations x, y, and name
toPos= zip [0..] . map unPoint2 . V.toList . _positions

-- the minimal triples for the half quad edges hq
-- gives pairs of start - end node, which produce two hq 
-- zip with id for the hqs
toEdge :: Triangulation p r -> [(Int, Int)]
toEdge = concatMap edgesPerNode . zip [0..]. map CL.toList . V.toList . _neighbours 

unPoint2 (Point2 x y :+ c) = (x, y, c)

edgesPerNode (s,[]) = []
edgesPerNode (s,t:ts) = (s,t): edgesPerNode (s,ts)
-- this gives all the half-quad-edges, i.e. 
-- one for (hq id, start, s), (hq id+, end, t), etc. 
-- the faces must be reconstructed from following the halfquads 
-- around a face 
-- construct the hq for the face later

-- makeNode :: Int -> (Int -> (PtTuple)) ->  (ObjPoint, MorphPoint, ObjPoint)
-- -- | the first is a offset for the node id
-- -- | same for both nodes and edges 
-- -- | id for edge (s t)
-- makeNode offset (n, (x,y,i)) = 
--     [ (NodeTag (Node (offset + n)), xyMorph, PointTag (Point2 x y))
--     , (NodeTag (Node (offset + n))), nameMorph, NameTag (NameInt i)]

-- makeEdge (offset, s, t) = [(EdgeTag (Edge (s,t)), sMorph, NodeTag (Node (offset + s)))
--     , (EdgeTag (Edge (s,t)), sMorph, NodeTag (Node (offset + t)))]


outputTriangulation :: ErrIO () 
outputTriangulation = do 
    putIOwords ["the tests for subdivisions, primarily triangulation"]

--------- data for test 
type PtTuple a = (Double, Double, a)
-- toPoint2 :: [PtTuple] -> [Point 2 Double :+ Text]
toPoint2   = map (\(x,y,i) -> (Point2 x y :+ showT i)) 

qs = [(Point2  0 0) :+ 'a' , Point2  1.5 1.5 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 0  :+ 'd']


tri_qs = delaunayTriangulation  . NE.fromList $ qs 
pos_qs = toPos tri_qs -- > (1,(0.0,0.0,'a')),(2,(1.5,1.5,'b')),(3,(0.0,2.0,'c')),(4,(2.0,0.0,'d'))]
edge_qs = toEdge tri_qs

-- pos_qs --[(0,(0.0,0.0,'a')),(1,(1.5,1.5,'b')),(2,(0.0,2.0,'c')),(3,(2.0,0.0,'d'))]
-- edge_qs -- [[(0,2),(0,1),(0,3)],[(1,3),(1,0),(1,2)],[(2,1),(2,0)],[(3,0),(3,1)]]

twoT :: [PtTuple Int]
twoT = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]
-- test points - x, y, id 
-- pos_two -> [(1,(0.0,0.0,11)),(2,(1.5,1.5,12)),(3,(0.0,2.0,13)),(4,(2.0,0.0,14))]
-- edge_two -> [[(1,2),(1,1),(1,3)],[(2,3),(2,0),(2,2)],[(3,1),(3,0)],[(4,0),(4,1)]]
tri_two = toTri twoT --  delaunayTriangulation . NE.fromList . toPoint2 $ twoT
pos_two :: [(Int, (Double, Double, Text))]
pos_two = toPos tri_two
edge_two :: [(Int, Int)]
edge_two = toEdge tri_two

-- tri_two
-- Triangulation {_vertexIds = fromList [(Point2 0.0 0.0,0),(Point2 0.0 2.0,2),(Point2 1.5 1.5,1),(Point2 2.0 0.0,3)], 
-- _positions = [Point2 0.0 0.0 :+ 11,Point2 1.5 1.5 :+ 12,Point2 0.0 2.0 :+ 13,Point2 2.0 0.0 :+ 14], _neighbours = [fromList [2,1,3],fromList [3,0,2],fromList [1,0],fromList [0,1]]}
-- pos_tri -- [(0,(0.0,0.0,11)),(1,(1.5,1.5,12)),(2,(0.0,2.0,13)),(3,(2.0,0.0,14))]
-- edge_tri -- [[(0,2),(0,1),(0,3)],[(1,3),(1,0),(1,2)],[(2,1),(2,0)],[(3,0),(3,1)]]

-- the rest not needed 
qtwo = toPoint2 twoT
threeT = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]
-- tests points used initially
tri_three = toTri threeT
pos_three = toPos tri_three 
edge_three = toEdge tri_three
-- tri_three -- Triangulation {_vertexIds = fromList [(Point2 0.0 0.0,0),(Point2 0.0 3.0,4),(Point2 3.0 0.0,1),(Point2 3.0 5.0,3),(Point2 4.0 2.0,2)], 
--     _positions = [Point2 0.0 0.0 :+ 21,Point2 3.0 0.0 :+ 22,Point2 4.0 2.0 :+ 23,Point2 3.0 5.0 :+ 24,Point2 0.0 3.0 :+ 25], 
--     _neighbours = [fromList [4,1],fromList [0,4,2],fromList [1,4,3],fromList [2,4],fromList [2,1,0,3]]}
-- pos_three -- [(0,(0.0,0.0,21)),(1,(3.0,0.0,22)),(2,(4.0,2.0,23)),(3,(3.0,5.0,24)),(4,(0.0,3.0,25))]
-- edge_three -- [[(0,4),(0,1)],[(1,0),(1,4),(1,2)],[(2,1),(2,4),(2,3)],[(3,2),(3,4)],[(4,2),(4,1),(4,0),(4,3)]]

-- -- rest preparation
-- t1 :: Triangulation Char Double
-- t1 = delaunayTriangulation . NE.fromList $ qs 
-- -- verts1 :: Vector (CList VertexID)
-- verts1 = _neighbours t1
-- -- pos1 = edgesAsPoints t1 -- gives all possible pairs
-- -- po11 :: [(Point 2 Double :+ Char)]
-- -- pos1 ::  Vector (Point 2 Double :+ Char)
-- pos1 =  _positions t1
-- -- pos1l :: [a]
-- -- pos1l =  fromJust . Vector.fromList .  CL.fromList $ pos1
-- pos1l =  V.toList pos1
-- unPoint2 (Point2 x y :+ c) = (x, y, c)
-- -- unLoc (p1,p2) = (unPoint2 p1, unPoint2 p2)
-- pos1m :: [(Double, Double, Char)]
-- pos1m = map unPoint2  pos1l
-- pos1mx = zip [0..] pos1m  
-- -- ready to convert to node triples with the local index first arg
-- -- then x,y, and the name given in the input for the triangulation 
-- pos_two = toPos tri_two 
--     -- zip [1..] . V.toList . _positions . delaunayTriangulation . NE.fromList . toPoint2 $ twoT


-- verts1l :: [CL.CList VertexID]
-- verts1l = V.toList verts1 
-- verts1ll :: [[Int]]
-- -- verts1ll :: [[VertexID]]
-- verts1ll = map CL.toList  verts1l   -- VertexID is Int
-- ver1mx :: [(Int, [Int])]
-- ver1mx = zip [0..] verts1ll
-- edgesPerNode :: (a, [b]) -> [(a, b)]
-- edgesPerNode (s,[]) = []
-- edgesPerNode (s,t:ts) = (s,t): edgesPerNode (s,ts)
-- -- this gives all the half-quad-edges, i.e. 
-- -- one for (hq id, start, s), (hq id+, end, t), etc. 
-- -- the faces must be reconstructed from following the halfquads 
-- -- around a face 
-- edgePairs = map edgesPerNode ver1mx


-- edge_two = toEdge tri_two 