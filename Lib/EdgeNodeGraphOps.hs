-----------------------------------------------------------------------------
--
-- Module      :  building edge node graph data in the triple store  
{- a graph with half-edges (preparing for quad edges in guibas stolfi triangulations

two objects: EdgeTag NodeTag 
two morphism: s :: T -> S and t :: T -> S (see lavwere)

try to build operations s and t to work in state monad

graph 1 - e - 2 - f - 3 

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
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib.EdgeNodeGraphOps
    -- (ObjST (..)
    -- , MorphST
    -- -- , MorphST'(Left, Right)
    -- -- , Left, Right
    -- , Wobj(..), Bobj(..)
    -- -- , either, distribute
    -- , b', w', h, k
    -- , T(..), S(..)
    -- , makeEdgeSpace, makeEdgeBusiness
    -- -- for test 
    -- , pageST_withTriples
    -- , pageST_withTriplesBusiness
    -- , pageSTproductCombines
    -- )    
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- end 

import UniformBase 
import Control.Monad.State
    ( MonadState(get), evalState, runState, State )

-- import Lib.Rules
-- import Vault.Values
-- import Data.List.Extra
-- import Data.Bifunctor (bimap)
-- import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat
    ( Action(..),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind) )
import Lib.EdgeNodeGraph 

-- findEx1 =  (id_find  (Just $ NodeTag . Node $ 1, Just sMorph, Nothing) )

-- findEx2 = (id_find_1st (Node 1))

-- id_find :: CatStoreQ -> StoreStateMonad [CPoint ObjST MorphST]
-- ^ a monadic wrapper for catStoreFind applied to state
find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
        (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
find t = do 
    c <- get

    let res = catStoreFind t c 
    -- (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing) c
    return  res 

testQuery :: (Maybe ObjST, Maybe a1, Maybe a2)
testQuery = (Just . NodeTag . Node $ 1, Nothing, Nothing) 
-- id_find_1st :: Node -- ^ 
--   -> StoreStateMonad [CPoint o m2]


find_node_edge :: () =>  Node -> StoreStateMonad  (CPoint ObjST MorphST)
-- ^ start with node get edge
find_node_edge i = do 
        r1  <- find  (Just . NodeTag $ i, Nothing, Nothing) 
        return . head $ r1

find_node_edge2 :: () =>  Node -> StoreStateMonad  (ObjST)
-- ^ start with node get edge
find_node_edge2 i = do 
        r1  <- find  (Just . NodeTag $ i, Nothing, Nothing) 
        return . getTarget3  $ r1


sFun :: () =>  Node -> StoreStateMonad  (Edge)
-- ^ start with node get edge
-- get the s related edge, fails on relation
sFun i = do 
        r1  <- find  (Just . NodeTag $ i, Just sMorph, Nothing) 
        return . unEdgeTag . getSingle3  $ r1
sRel :: () =>  Node -> StoreStateMonad  ([Edge])
-- ^ start with node get edge
-- get the s related edge, fails on relation
sRel i = do 
        r1  <- find  (Just . NodeTag $ i, Just sMorph, Nothing) 
        return . map unEdgeTag . map trd3  $ r1
tFun :: () =>  Node -> StoreStateMonad  (Edge)
-- ^ start with node get edge using t
tFun i = do 
        r1  <- find  (Just . NodeTag $ i, Just tMorph, Nothing) 
        return . unEdgeTag . getSingle3  $ r1
tRel :: () =>  Node -> StoreStateMonad  ([Edge])
-- ^ start with node get edge using t
tRel i = do 
        r1  <- find  (Just . NodeTag $ i, Just tMorph, Nothing) 
        return . map unEdgeTag . map trd3  $ r1



sInvFun :: () =>  Edge -> StoreStateMonad  Node
-- ^ start with edge get node using s
sInvFun i = do 
        r1  <- find  (Nothing, Just sMorph, Just . EdgeTag $ i) 
        return . unNodeTag . getSingle1  $ r1
sInvRel :: () =>  Edge -> StoreStateMonad  [Node]
-- ^ start with edge get node using s
sInvRel i = do 
        r1  <- find  (Nothing, Just sMorph, Just . EdgeTag $ i) 
        return . map unNodeTag . map fst3  $ r1
tInvFun :: () =>  Edge -> StoreStateMonad  Node
-- ^ start with edge get node using t 
tInvFun i = do 
        r1  <- find  (Nothing, Just tMorph, Just . EdgeTag $ i) 
        return . unNodeTag . getSingle1  $ r1
tInvRel :: () =>  Edge -> StoreStateMonad  [(Node)]
-- ^ start with edge get node using t 
tInvRel i = do 
        r1  <- find  (Nothing, Just tMorph, Just . EdgeTag $ i) 
        return . map unNodeTag . map fst3  $ r1


isSingleton a = 1 == length a
openSingleton [a] = a
openSingleton x = errorT ["openSingleton - not", showT x ]

getSingle1 :: [(ObjST, Either S T, ObjST)] -> ObjST
getSingle1 = fst3 . openSingleton
-- ^ unwrap the first if singleton 
getSingle3 = trd3 . openSingleton


part2 :: ErrIO ()
part2 = do 
    putIOwords ["\n part 2 trying to get perfect functions and inverses between node and edge (s and t"]

    let datafn = makeAbsFile "/home/frank/CoreConcepts/edgeNode123"
    nt1 <- read8 datafn catStoreFileType 
    putIOwords ["the store", showT nt1]

    let res1 = evalState  (find testQuery) nt1
    putIOwords  ["the result of testQuery", showT res1]
    let res2 = evalState  (find_node_edge (Node 1)) nt1
    putIOwords  ["the result of node edge query", showT res2]
    let res3 = evalState  (find_node_edge2 (Node 1)) nt1
    putIOwords  ["the result of node edge query", showT res3]
    let res4 = evalState  (sFun (Node 1)) nt1
    putIOwords  ["the result of node edge query", showT res4]
    let res5 = evalState  (tInvFun (Edge 'e')) nt1
    putIOwords  ["the result of edge node t query", showT res5]
    let res6 = evalState  (tInvFun =<< sFun (Node 1)) nt1
    -- to make it like . composition (otherwse use >>= )
    putIOwords  ["the result of node - s - tinv - node", showT res6]
    putIOwords  ["the result of tinv 'e'  edge"
            , showT $ evalState (tInvRel (Edge 'e')) nt1]
    putIOwords  ["the result of edge e - tInv - node - s - edge"
            , showT $ evalState (sFun =<< tInvFun (Edge 'e')) nt1]
