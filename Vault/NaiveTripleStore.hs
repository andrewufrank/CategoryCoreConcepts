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


type Val = ValueSum 

-- | a store for typed triples 
class () => TripleStore o p v  where
    tsempty :: [(o,p,v)]
    tsinsert :: (o,p,v) -> [(o,p,v)] -> [(o,p,v)]
    tsdel :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsfind :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsbatch :: [Action (o,p,v)] -> [(o,p,v)] -> [(o,p,v)]



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


pageNT :: IO ()
pageNT = do
    putIOwords ["\n [page NT"]


toCond :: Eq v => Maybe v -> (v -> Bool)
toCond (Nothing) = const True
toCond (Just v) = (v==)

toMaybes :: (a1, a2, a3) -> (Maybe a1, Maybe a2, Maybe a3)
toMaybes (s,p,o) = (Just s, Just p, Just o)

filterTriple :: (Eq a, Eq b, Eq v) =>
            (Maybe a, Maybe b, Maybe v) -> (a, b, v) -> Bool
filterTriple (mo, mp, mv) t = (toCond mo . fst3 $ t)
                            && (toCond mp . snd3 $ t)
                            && (toCond mv . trd3 $ t)



-------------- test data

data TestRel = T1 | T2 deriving (Show, Read, Ord, Eq)
type Triple = (Key, TestRel, Val) -- deriving (Show, Read, Ord, Eq)

ts0, ts1 :: [Triple]
ts0 = tsempty
ts1 = tsinsert (k1,r1,v1) ts0
ts2 :: [(Key, TestRel, ValueSum)]
ts2 = tsinsert t2 ts1

t1 :: (Key, TestRel, ValueSum)
t1 = (k1, r1, v1)
t2 :: (Key, TestRel, ValueSum)
t2= (mkkey "t2", r1, mktext "label2")
t3 :: (Key, TestRel, ValueSum)
t3= (mkkey "t3", r1, mktext "label3")
m1 :: (Maybe Key, Maybe a1, Maybe a2)
m1 = (Just (mkkey "t1"), Nothing, Nothing)
m2 :: (Maybe Key, Maybe a1, Maybe a2)
m2 = (Just (mkkey "t2"), Nothing, Nothing)
k1 :: Key
k1 = mkkey "t1"
r1 :: TestRel
r1 = T1
v1 :: ValueSum
v1 = mktext "label1"