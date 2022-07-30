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
-- import Control.Monad.State

-- import Vault.Triple4cat
    -- ( Action(Ins),
    --   CatStores(catStoreBatch, catStoreEmpty),
    --   CatStore,
    --   find2rel,
    --   find2fun,
    --   MorphSel(Forward, Inv) )

-- import GIS.Category
-- import Storable.Sobj  -- the storable objects 
-- import Storable.Srel -- the storable relations
import Storable.Store 

import Uniform.TesselationHalfQuads
-- import Uniform.Point2dData
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


makeTripFace :: Int -> FaceHQ -> StoreElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (FaceTag . Face $ i, xyMorph, PointTag . fromV2toP2d . circumcenter $ fhq)


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
