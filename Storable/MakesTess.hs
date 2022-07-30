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

import Uniform.TesselationHalfQuads
import Uniform.Point2dData
-- import  Uniform.DelaunayTriples hiding (HqID(..)) 
-- import  qualified Uniform.DelaunayTriples as Tess
-- -- should export all what is needed

        
{- 
data NodeHQ = NodeHQ V2d
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {circumcenter ::V2d} deriving (Show, Read, Ord, Eq, Generic, Zeros)

data HQ = HQ 
    { node:: Int    -- ^ end of hq (start or end of edge)
    , face::Maybe Int -- ^ face right of the hq
    , twin::Int     -- the other hq for the edge
    , halflength :: Double  -- the half of the length of the edge
    } 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQs       :: [HQ]      -- ^ the tileface starting and ending, alternating
        } deriving Show

-}

data TesselationHQtriples = TesselationHQtriples 
    { _NodesTrip :: [StoreElement]
    , _FacesTrip :: [StoreElement]
    , _HQtrips   :: [StoreElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

makeTripNode :: Int -> NodeHQ -> StoreElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
makeTripNode  i (NodeHQ v2x) = (NodeTag . Node $ i, xyMorph, PointTag . fromV2toP2d $ v2x)

-- makeTripNode  i v = (fromGeomIDnode oid, xyMorph, PointTag . fromList2P2d $ val)
--     -- where
-- fromGeomIDnode (Tess.N i)    = NodeTag . Node . fromIntegral  $ i

makeTripFace :: Int -> FaceHQ -> StoreElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (FaceTag . Face $ i, xyMorph, PointTag . fromV2toP2d . circumcenter $ fhq)

-- data HQ = HQ 
--     { node:: Int    -- ^ end of hq (start or end of edge)
--     , face::Maybe Int -- ^ face right of the hq
--     , twin::Int     -- the other hq for the edge
--     , halflength :: Double  -- the half of the length of the edge
--     } 
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)

makeTripHq :: Int -> Int -> HQ -> [StoreElement]
-- convert the HQ data to storeelements
makeTripHq offset i hq = catMaybes [hqnode, hqface, hqtwin, hqhalflength]
    where
        hqid = HQTag . Hq $ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreElement
        hqnode = Just $ (hqid, hqNodeMorph, NodeTag . Node   . (+offset) . node $ hq)
        hqface = fmap  (\fi -> (hqid, hqFaceMorph, FaceTag . Face  . (+offset) $ fi)) (face hq)
        hqtwin = Just $ (hqid, twinMorph, HQTag . Hq  . (+offset) . twin $ hq)
        hqhalflength = Just $ (hqid, distanceMorph, LengthTag . Length . halflength $ hq) 



hqToTrip :: Int -> TesselationHQ ->  TesselationHQtriples
hqToTrip offset teshq  = TesselationHQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQs teshq)
    } 


mainMakeTess :: ErrIO () 
mainMakeTess = do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tess <- liftIO $ delaunay2 fourV2    
    let trips = hqToTrip 400 . toHq1 $ tess 
    putIOwords ["triples produces\n", showT trips]

    return ()
-----------------old

-- ------------------------------------------------------------------
-- -- the makes for tesselations (delaunay etc.)
-- -- produce a list of storeElements 
-- -- they are colled with just the Int id and put the type around it

-- -- store node

-- -- make returns one, list only if requred

-- makeFaceSurface :: (FaceID, Double) -> StoreElement
-- -- | convert trip_surface2, used hqf
-- makeFaceSurface (fid, val) = (FaceTag . Face . fromIntegral . unFaceID $ fid, surfacedMorph, AreaTag . Area $ val ) 

-- makeXYnode  :: (NodeID, [Coord]) -> StoreElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
-- makeXYnode  (oid, val) = (fromGeomIDnode oid, xyMorph, PointTag . fromList2P2d $ val)
--     -- where
-- fromGeomIDnode (Tess.N i)    = NodeTag . Node . fromIntegral  $ i

-- makeXYface  :: (FaceID, [Coord]) -> StoreElement
-- -- | convert trip_xy   hqfxy,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
-- makeXYface  (oid, val) = (fromGeomIDface oid, xyMorph, PointTag . fromList2P2d $ val)
--     -- where
--         -- fromGeomID (Tess.N i)    = NodeTag . Node . fromIntegral  $ i        
-- fromGeomIDface (Tess.F i)    = FaceTag . Face . fromIntegral  $ i

-- makeHQlength :: (Tess.HqID, Double) -> [StoreElement]
-- -- from trip_hq_lengthX and 2X 
-- -- half the length of the edge between two nodes
-- makeHQlength (hqid, val) = [(HQTag . Hq . fromIntegral . Tess.unHqID $ hqid, DistTag Distant, LengthTag . Length $ val)]

-- fromGeomID2hq :: Tess.HqID -> ObjPoint
-- fromGeomID2hq a@(Tess.Hq i)= HQTag . Hq . fromIntegral $ i


-- makeHQnode :: (Tess.HqID, Tess.NodeID) -> [StoreElement]
-- -- make the HQ with the node and the faces. from trip_hqs_faces
-- makeHQnode  (hqid, a@(Tess.N id)) = [(fromGeomID2hq hqid, hqNodeMorph, fromGeomIDnode  a)]
-- makeHQhq (hqid, a@(Tess.Hq id)) = [(fromGeomID2hq hqid, hqFaceMorph, fromGeomID2hq  a)]
-- makeHQface  (hqid, a@(Tess.F id)) = [(fromGeomID2hq hqid, hqFaceMorph, fromGeomIDface  a)]

-- makeTesselation :: Integer -> Tess.Tesselation -> [StoreElement]
-- -- | make all the triples for a tesselation
-- -- set the offset for all 
-- makeTesselation offs res4 = concat [se_faceSurface, se_xynode, se_xyface, se_hqlength, se_hqs1, se_hqs2, se_hqs3]
--     where
--         hqfaces = trip_surface2 offs res4       -- faces surface areas
--         se_faceSurface =  map makeFaceSurface hqfaces :: [StoreElement]

--         hqnxy = trip_xy N 400 (vertices res4)  -- the node coords
--                 -- should not be N from geometry
--         se_xynode = map makeXYnode hqnxy::[StoreElement]

--         hqfxy = trip_xy F 400 (center res4)  -- the face coords
--                 -- should not be N from geometry
--         se_xyface = map makeXYface hqfxy ::[StoreElement]

--         hqlength = trip_hq_lengthX 400 res4
--         hqlength2 = trip_hq_length2X 400 res4
--         se_hqlength = concat $ map makeHQlength (hqlength ++ hqlength2)
--                                 :: [StoreElement]

--         hqs = trip_hqs_faces 400 res4
--         se_hqs1 = concat $ map makeHQnode (concat . map fst3 $ hqs)
--         se_hqs2 = concat $ map makeHQhq   (concat . map snd3 $ hqs)
--         se_hqs3 = concat $ map makeHQface (concat . map trd3 $ hqs)

