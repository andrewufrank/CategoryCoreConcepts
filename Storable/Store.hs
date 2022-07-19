-----------------------------------------------------------------------------
--
-- Module      :  the store for the category in GIS.Category
{-  

 construct the two sum types and the storage

 the s-obj are in the subj (o) and value (v) values in the triples 
 
 the s-morphism are the relations in the triple store 
 (the p = predicate) 
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

module Storable.Store
(module Storable.Store 
    , module Storable.Sobj 
    , module Storable.Srel

) where

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
import Storable.Sobj  -- the storable objects 
import Storable.Srel -- the storable relations

----------- the category





-- code depending on MonadState
type Store = CatStore ObjPoint MorphPoint
type StoreStateMonad = State Store  
type StoreErrIO = StateT Store ErrIO

