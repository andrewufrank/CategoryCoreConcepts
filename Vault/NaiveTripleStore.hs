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
{-# LANGUAGE DeriveFunctor    #-}

module Vault.NaiveTripleStore 
--     (NaiveTriples (..)
--     , Store (..)
--     , newNaiveStore
--     -- , storeCopyKeyData
-- --        module Vault.NGtriplestore
-- --    ,  Rel4Test  (..)
--     , Key (..) ,mkkey , unkey
--     , Val 
--     , Vals (..) 
--     -- unVal4, isVal4
--     -- , B4val (..), proxyB4val, proxyFloat
--     , Row (..)   
--        )    
    where

import UniformBase
-- import Uniform.Error
-- import Data.List.Extra

import Vault.Value



data TestRel = T1 | T2 deriving (Show, Read, Ord, Eq)

-- class Store0 row where 
--     ntInserto :: row -> Store row -> Store Row

-- nextKey (Key i) = Key (i+1)

type Val = ValueSum 

-- | a store for typed triples 
class () => TripleStore o p v  where
    tsempty :: [(o,p,v)]
    tsinsert :: (o,p,v) -> [(o,p,v)] -> [(o,p,v)]
    tsdel :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsfind :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsbatch :: [Action (o,p,v)] -> [(o,p,v)] -> [(o,p,v)]

type Triple = (Key, TestRel, Val) -- deriving (Show, Read, Ord, Eq)
-- type Query = (Maybe Key, Maybe TestRel, Maybe Val)
data Action a = Ins a | Del a
        deriving (Show, Read, Ord, Eq)

instance TripleStore Key TestRel Val where 
    tsempty = []
    tsinsert t@(o,p,v) = ( t :)
    tsdel t@(mo, mp, mv) = filter (not . filterTriple t )
    tsfind t@(mo, mp, mv) =  filter (filterTriple t)
    tsbatch [] ts = ts
    tsbatch ((Ins t) : as) ts = tsinsert t . tsbatch as $ ts
    tsbatch ((Del t) : as) ts = tsdel (toMaybes t) . tsbatch as $ ts

    -- ntInsert:: Key -> rel -> Val ->  Store rel ->  Store rel
    -- ntInsertRow:: row ->  Store [row] ->  Store [row]
    -- ntDeleteAll :: Key -> Store rel ->  Store rel
    -- ntDeleteVal :: Key -> rel ->   Store rel ->  Store rel
    -- ntFind :: Maybe Key -> Maybe rel -> Maybe Val -> Store rel -> [Row rel]
    -- ntFind2 :: (Key -> Bool)  -> (rel -> Bool) -> (Val -> Bool) -> Store rel -> [Row rel]
    -- ntFindKR_V :: Key -> rel -> Store rel -> [Val]
    -- ntFindRV_K :: rel -> Val -> Store rel -> [Key]
    -- ntFindKR_V k r = map rv . ntFind (Just k) (Just r) Nothing
    -- ntFindRV_K r  v = map rk . ntFind Nothing (Just r) (Just v)

-- data Row rel = Row {rk::Key, rr::rel, rv::Val}  deriving (Show, Read, Eq)

pageNT :: IO ()
pageNT = do
    putIOwords ["\n [page NT"]



-- newtype Store r = Store  r deriving (Show, Read, Eq, Functor)
-- -- unStore :: Store r -> [Row r]
-- -- unStore (Store rs) = rs
-- -- newNaiveStore :: (Monoid r) => Store r
-- -- newNaiveStore = mempty

-- -- instance Semigroup (Store r)
-- -- instance (Monoid r) => Monoid (Store r) where mempty = Store mempty

toCond :: Eq v => Maybe v -> (v -> Bool)
toCond (Nothing) = const True
toCond (Just v) = (v==)

toMaybes (s,p,o) = (Just s, Just p, Just o)

filterTriple (mo, mp, mv) t = (toCond mo . fst3 $ t)
                            && (toCond mp . snd3 $ t)
                            && (toCond mv . trd3 $ t)

-- instance (Eq row) => NaiveTriples  row  where
--     -- ntInsert k r v   = Store . (Row k r v :) . unStore
--     -- ntInsertRow row1 = Store . (row1 :) . unStore
--     ntInsertRow row1 = fmap (cons row1)
--     -- ntFind mk mr mv   = ntFind2 (toCond mk) (toCond mr) (toCond mv)
--     -- ntFind2 ck cr cv   = filter (ck . rk) . filter (cr . rr) . filter (cv . rv) . unStore
--     -- ntDeleteAll k   =  Store . filter ((k/=).rk) . unStore
--     -- ntDeleteVal k p  =  Store . filter (\r -> ((k/=).rk $ r) || ((p/=).rr $ r)) . unStore


-- -- | copy all data with a given key to a new key
-- -- storeCopyKeyData :: (Eq rel) => Key -> Key -> Store rel -> Store rel
-- -- storeCopyKeyData kold knew v =  Store $ newvals ++ st
-- --     where   oldvals = ntFind (Just kold) Nothing Nothing $ v
-- --             newvals = map (\abc -> Row knew (rr abc) (rv abc)) oldvals -- :: [Row rel]
-- --             st = unStore v -- :: [Row rel]


ts0, ts1 :: [Triple]
ts0 = tsempty
ts1 = tsinsert (k1,r1,v1) ts0
ts2 = tsinsert t2 ts1

t1 = (k1, r1, v1)
t2= (mkkey "t2", r1, mktext "label2")
t3= (mkkey "t3", r1, mktext "label3")
m1 = (Just (mkkey "t1"), Nothing, Nothing)
m2 = (Just (mkkey "t2"), Nothing, Nothing)
k1 = mkkey "t1"
r1 = T1
v1 = mktext "label1"