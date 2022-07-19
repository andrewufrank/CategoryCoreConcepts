-----------------------------------------------------------------------------
--
-- Module      :  retrieve the data from the store  
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

module Storable.Retrieve
 
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
-- the functions and relatiosn to acess the store 

xyFun :: (MonadState (Store) m) =>  Node -> m  (Point2d)
-- ^ start with node get point (x y coordinates)
xyFun i = find2fun Forward (NodeTag i) xyMorph unPointTag

sFun :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with node get edge
sFun i = find2fun Forward (EdgeTag i) sMorph unNodeTag 

-- sRel :: (MonadState (Store) m) => Edge -> m [Edge]
-- sRel i = find2rel Forward (EdgeTag i) sMorph unEdgeTag 

tFun :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with node get edge using t
tFun i = find2fun Forward (EdgeTag i) tMorph unNodeTag 

-- tRel :: (MonadState (Store) m) =>  Edge -> m  [Edge]
-- -- ^ start with node get edge using t
-- tRel i = find2rel Forward (NodeTag i) tMorph unEdgeTag 

-- sInv :: (MonadState (Store) m) =>  Edge -> m  Edge
-- -- ^ start with edge get node using s
-- sInv i = find2fun Inv (EdgeTag i) sMorph unNodeTag 

sInvRel :: MonadState (Store) m =>  Node -> m [Edge]
sInvRel i = find2rel Inv (NodeTag i) sMorph unEdgeTag

-- tInv :: (MonadState (Store) m) =>  Edge -> m  Edge
-- -- ^ start with edge get node using t 
-- tInv i = find2fun Inv (EdgeTag i) tMorph unNodeTag

tInvRel :: (MonadState (Store) m) =>  Node -> m  [Edge]
-- ^ start with edge get node using t 
tInvRel i = find2rel Inv (NodeTag i) tMorph unEdgeTag

sCostFun :: MonadState (Store) m => Edge -> m Cost
sCostFun i = find2fun Forward (EdgeTag i) scMorph unCostTag

