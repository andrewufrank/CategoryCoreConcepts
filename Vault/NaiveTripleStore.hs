-----------------------------------------------------------------------------
--
-- Module      :  a naive implementation of  a triple store
-- i.e. the minimal and most simple implementation as list
-- the triples are Key - Rel - Val (to reduce confusion later...)
-- corresponding to Subj - Prop - Ojb
-- read and write ok
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Vault.NaiveTripleStore (NaiveTriples (..)
    , Store, newNaiveStore
    -- , storeCopyKeyData
--        module Vault.NGtriplestore
--    ,  Rel4Test  (..)
    , Key (..) ,mkkey , unkey
    , Val 
    , Vals (..) 
    -- unVal4, isVal4
    -- , B4val (..), proxyB4val, proxyFloat
    , Row (..)   
       )    where

import UniformBase
-- import Uniform.Error

import Vault.Value


-- nextKey (Key i) = Key (i+1)

type Val = ValueSum 

-- | a store for typed triples parametrized in the rel names
class (Eq rel) => NaiveTriples rel   where
    ntInsert:: Key -> rel -> Val ->  Store rel ->  Store rel
    ntDeleteAll :: Key -> Store rel ->  Store rel
    ntDeleteVal :: Key -> rel ->   Store rel ->  Store rel
    ntFind :: Maybe Key -> Maybe rel -> Maybe Val -> Store rel -> [Row rel]
    ntFind2 :: (Key -> Bool)  -> (rel -> Bool) -> (Val -> Bool) -> Store rel -> [Row rel]
    ntFindKR_V :: Key -> rel -> Store rel -> [Val]
    ntFindRV_K :: rel -> Val -> Store rel -> [Key]
    ntFindKR_V k r = map rv . ntFind (Just k) (Just r) Nothing
    ntFindRV_K r  v = map rk . ntFind Nothing (Just r) (Just v)

data Row rel = Row {rk::Key, rr::rel, rv::Val}  deriving (Show, Read, Eq)

newtype Store r = Store  [Row r] deriving (Show, Read, Eq)
unStore :: Store r -> [Row r]
unStore (Store rs) = rs
newNaiveStore :: Store r
newNaiveStore = Store []


toCond :: Eq v => Maybe v -> (v -> Bool)
toCond (Nothing) = const True
toCond (Just v) = (v==)

instance (Eq rel) => NaiveTriples  rel  where
    ntInsert k r v   = Store . (Row k r v :) . unStore
    ntFind mk mr mv   = ntFind2 (toCond mk) (toCond mr) (toCond mv)
    ntFind2 ck cr cv   = filter (ck . rk) . filter (cr . rr) . filter (cv . rv) . unStore
    ntDeleteAll k   =  Store . filter ((k/=).rk) . unStore
    ntDeleteVal k p  =  Store . filter (\r -> ((k/=).rk $ r) || ((p/=).rr $ r)) . unStore


-- | copy all data with a given key to a new key
-- storeCopyKeyData :: (Eq rel) => Key -> Key -> Store rel -> Store rel
-- storeCopyKeyData kold knew v =  Store $ newvals ++ st
--     where   oldvals = ntFind (Just kold) Nothing Nothing $ v
--             newvals = map (\abc -> Row knew (rr abc) (rv abc)) oldvals -- :: [Row rel]
--             st = unStore v -- :: [Row rel]

