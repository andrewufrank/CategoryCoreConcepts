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

module Vault.Triple4cat_test
    where


import UniformBase
-- import Vault.NaiveTripleStore
-- import Vault.Object 
import Vault.Value
import Vault.Triple4cat

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

test_empty = assertEqual ("VaultK []") (showT (v0))

test_insert1 = assertEqual (concat'["VaultK [",  res1, "]"]) (showT v1)
test_insert2 = assertEqual (concat'["VaultK [", res2, ",", res1, "]"]) (showT v2)

test_batch_insert = assertEqual (concat'["VaultK [", res2, ",", res1, "]"])
    (showT v2)
res1 :: Text
res1 = "(Key \"x1\",Edge,VT (Value \"label x1\"))"
res2 = "(Key \"x2\",Edge,VT (Value \"label x2\"))"
res21 = concat'["[", res2, ",", res1, "]"]

-- test_batch_insert21 :: IO ()
-- test_batch_insert21 = assertEqual (concat'["[", res2, ",", res1, "]"])
--     (showT . tsbatch [Ins t2, Ins t1] $ ts0)

-- test_find = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Nothing, Nothing) ts1))
-- test_find2 = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Just r1, Nothing) ts1))

test_del0 = assertEqual (concat'["VaultK [", res2, ",", res1, "]"])
    (showT  v2)
test_del1 = assertEqual (concat'["VaultK [",   res1, "]"])
    (showT v3)

-- test_delBatch = assertEqual (concat'["[", res1, "]"] ) 
--     (showT $ tsbatch [Del t2] ts2)



