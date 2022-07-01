 -----------------------------------------------------------------------------
--
-- Module      :  Test Vault
-- Copyright   :
--
-- | the test for Vault to avoid problems there

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Vault.Vault_test
    where


import UniformBase
import Vault.NaiveTripleStore
import Vault.Object 
import Vault.Value

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)


test_empty = assertEqual ([]) (ntQuery Nothing Nothing Nothing newNaiveStore :: [Row GraphRels])
-- -- an empty store must contain nothing 

-- test_insertOne = assertEqual undefined
-- --             (Store [Row{rk = Key "t1", rr = L, rv = VT (Value "label1")}]) 
-- --             (ntInsertRow row1 $
-- --                             newNaiveStore :: Store [Row GraphRels])

-- test_insert = assertEqual 
--             ([Row{rk = Key "t1", rr = L, rv = VT (Value "label1")}]) (ntFind Nothing Nothing Nothing . 
--                             -- ntInsert k1 r1 v1 $
--                             ntInsertRow row1 $
--                             newNaiveStore :: [Row GraphRels])
-- row1 = Row k1 r1 v1 :: Row GraphRels
k1 = mkkey "t1"
r1 = L
v1 = mktext "label1"