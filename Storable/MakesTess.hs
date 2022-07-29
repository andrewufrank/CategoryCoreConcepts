-----------------------------------------------------------------------------
--
-- Module      :  makeXX are the functions to store sobj 
--                  for Tesselations only 

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

module Storable.MakesTess
 
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

import Uniform.DelaunayTriples -- hiding (Hq (..))
-- should export all what is needed

------------------------------------------------------------------
-- the makes for tesselations (delaunay etc.)
-- produce a list of storeElements 
-- they are colled with just the Int id and put the type around it

-- store node

makeFaceSurface :: (FaceID, Double) -> [StoreElement]
-- | convert trip_surface2, used hqf
makeFaceSurface (fid, val) = [(FaceTag . Face . fromIntegral . unFaceID $ fid, surfacedMorph, AreaTag . Area $ val )] 

makeXY  :: (a, Coord) -> [StoreElement]
-- | convert trip_xy   hqnx,   
-- a is NodeID or FaceID (for center )
-- note: the Face is the dual of the Node 
makeXY  (oid, val) = [(toSub oid, xyMorph, PointTag . Point2d . fromList2P2d $ val)]
    where 
        toSub (N i) = NodeTag . Node . fromIntegral . unNodeID $ i
        toSub (F i) = FaceTag . Face . fromIntegral . unFaceID $ i

-- makePoint :: Int -> Text -> Double -> Double -> [StoreElement]
-- -- | to create a node with the nodeid and the given name and x y 
-- makePoint i n x y = makeNode1 i n x y

-- makeNode ::  Int -> (Int, (Double, Double, Text)) ->  StoreElementList
-- -- | the first is a offset for the node id
-- -- | same for both nodes and edges 
-- -- | id for edge (s t)
-- makeNode ofs (i, (x,y, n)) = makeNode1 (ofs + 1) n x y 

-- -- the base make node with text name and two doubles 
-- makeNode1 ::  Int -> Text -> Double -> Double  ->  [StoreElement]
-- makeNode1  i n x y = 
--     [ (NodeTag node, xyMorph, PointTag (Point2d x y))
--     , (NodeTag node, namedMorph, NameTag (Name n))]
--     where node = Node i

-- makeHQ :: Int -> (Int, Int) -> [(ObjPoint, MorphPoint, ObjPoint)]
-- makeHQ offset (s, t) = 
--     [ (HQTag hqid1, sMorph, NodeTag (Node ( offset + s)))
--     -- , (HQTag hqid1, sMorph, NodeTag (Node (offset + t)))
--     , (HQTag hqid1, twinMorph, HQTag (hqid2))
--     -- , (HQTag hqid2, sMorph, NodeTag (Node ( offset + s)))
--     , (HQTag hqid2, sMorph, NodeTag (Node (offset + t)))
--     , (HQTag hqid2, twinMorph, HQTag (hqid1))
--     ]
--     where 
--         hqid1 = Hq $ offset + 100 * s + t
--         hqid2 = Hq $ offset + 100 * t + s

-- showT :: Node -> Node 
-- showT x = ShowT x