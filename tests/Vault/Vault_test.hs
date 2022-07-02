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
-- import Vault.Object 
import Vault.Value

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

test_empty = assertEqual ("[]") (showT (tsempty :: [Triple]))

test_insert = assertEqual (concat'["[", res1, "]"] ) (showT ts1)

test_batch_insert = assertEqual (concat'["[", res2, ",", res1, "]"])
    (showT . tsbatch [Ins t2] $ ts1)
res1 :: Text
res1 = "(Key \"t1\",T1,VT (Value \"label1\"))"
res2 = "(Key \"t2\",T1,VT (Value \"label2\"))"


test_find = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Nothing, Nothing) ts1))
test_find2 = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Just r1, Nothing) ts1))

-- test_empty = assertEqual ([]) (ntFind Nothing Nothing Nothing newNaiveStore :: [Row GraphRels])
-- -- an empty store must contain nothing 

-- test_insertOne = assertEqual 
--             (Store [Row{rk = Key "t1", rr = T1, rv = VT (Value "label1")}]) 
--             (ntInsertRow row1 $
--                             newNaiveStore :: Store [Row TestRel])

-- -- test_insert = assertEqual 
-- --             ([Row{rk = Key "t1", rr = L, rv = VT (Value "label1")}]) (ntFind Nothing Nothing Nothing . 
-- --                             -- ntInsert k1 r1 v1 $
-- --                             ntInsertRow row1 $
-- --                             newNaiveStore :: [Row GraphRels])
-- row1 = Row k1 r1 v1 :: Row TestRel


ts0, ts1 :: [Triple]
ts0 = tsempty
ts1 = tsinsert (k1,r1,v1) ts0

t1 = (k1, r1, v1)
t2= (mkkey "t2", r1, mktext "label2")
k1 = mkkey "t1"
r1 = T1
v1 = mktext "label1"