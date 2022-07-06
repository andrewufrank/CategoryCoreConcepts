-----------------------------------------------------------------------------
--
-- Module      :  building edge node graph data in the triple store  
{- a graph with half-edges (preparing for quad edges in guibas stolfi triangulations

two objects: EdgeTag NodeTag 
two morphism: s :: T -> S and t :: T -> S (see lavwere)

store in CatStore and statemonad 

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

module Lib.EdgeNodeGraph
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
import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat
    ( Action(..),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind) )

type MorphST  = Either S T  
-- ^ the morphism from NodeTag to EdgeTag 

sMorph :: Either S b
sMorph = Left S 
tMorph :: Either a T
tMorph = Right T 


data S = S  deriving (Show, Read, Ord, Eq, Generic)
data T = T  deriving (Show, Read, Ord, Eq, Generic)

data ObjST = NodeTag (NodeType Int) | EdgeTag (EdgeType Char) | ZZst
    deriving (Show, Read, Ord, Eq)
-- ^ the objects NodeTag and EdgeTag
instance Zeros ObjST where zero = ZZst

unEdgeTag :: ObjST -> Edge 
unEdgeTag (EdgeTag t) = t 
unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

unNodeTag :: ObjST -> Node
unNodeTag (NodeTag t) = t 
unNodeTag x = errorT ["unNodeTag - not a Node", showT x]

data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type Node = NodeType Int 
type Edge = EdgeType Char

-- type CPointST = CPoint ObjST MorphST 

makeEdgeFrom :: Int -> Char -> (ObjST, MorphST, ObjST)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeFrom o1 o2 = (NodeTag (Node o1), sMorph, EdgeTag (Edge o2))

makeEdgeTo :: Int -> Char ->   (ObjST, MorphST, ObjST)
makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))

-- s' :: () => CatStore ObjST MorphST -> Node ->   Edge
-- | get the edge from a given node, folling the s path
-- could eventually by multiple!
s' :: Eq b => CatStore ObjST (Either S b) -> Node   -> Edge 
s' cat ow =  unEdgeTag . getTarget3 . catStoreFind (Just . NodeTag $ ow, Just sMorph, Nothing) $ cat

ti' :: () => CatStore ObjST MorphST -> Edge ->   Node
-- | get the node from a given edge, folling the s path
-- could eventually by multiple!
ti' cat ow = unNodeTag .  getTarget1 . catStoreFind (Nothing, Just tMorph, Just . EdgeTag $ ow) $ cat

getTarget3 :: [(a, b, c)] -> c
-- | get target (pos3) from a singleton result 
-- just a helper
getTarget3 cps = trd3 . head  $ cps  

getTarget1 :: [(a, b, c)] -> a
-- | get target (pos 1 ) from a singleton result 
-- just a helper
getTarget1 cps = fst3 . head  $ cps 

-- data for test 
cat0 :: CatStore ObjST MorphST
cat0 = catStoreEmpty :: CatStore ObjST MorphST
cat2 :: CatStore ObjST MorphST
cat2 = catStoreBatch (
    [ Ins (makeEdgeFrom 1 'e')
    , Ins (makeEdgeTo   2 'e')
    , Ins (makeEdgeFrom 2 'f')
    , Ins (makeEdgeTo   3 'f')
    ]) cat0

e1 :: EdgeType Char
e1 = s'  cat2  (Node 1)

pageEdgeNodeGraph :: ErrIO ()
pageEdgeNodeGraph = do
    putIOwords ["\n pageEdgeNodeGraph"]
    putIOwords ["data for 1-2-3", showT cat2]
    putIOwords ["find edge starting at 1", showT . s'  cat2 $ (Node 1)]
    putIOwords ["e", showT e1]

    putIOwords ["find node from edge e", showT . ti'  cat2 $ e1]

    let datafn = makeAbsFile "/home/frank/CoreConcepts/edgeNode123"
    write8 datafn catStoreFileType cat2 

    putIOwords ["evalState id_find",   showT $ evalState findEx1 (cat2) ]
    -- putIOwords ["evalState id_find edge to node 1:", showT (evalState findEx2 (cat2))]

findEx1 =  (id_find  (Just $ NodeTag . Node $ 1, Just sMorph, Nothing) )

-- findEx2 = (id_find_1st (Node 1))

-- for writing to file
-- for typed files 
catStoreFileType :: TypedFile5 Text Store
catStoreFileType = makeTyped (Extension "tris")  :: TypedFile5 Text Store

instance TypedFiles7 Text Store where
  wrap7 = read . t2s
  unwrap7 = showT



-- for state monad
type StoreStateMonad = State Store  
type Store = CatStore ObjST MorphST

type CatStoreQuery o m = (Maybe o, Maybe m, Maybe o)
type CatStoreQ = CatStoreQuery ObjST MorphST

-- id_find :: CatStoreQ -> StoreStateMonad [CPoint ObjST MorphST]
-- ^ a monadic wrapper for catStoreFind applied to state
id_find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
        (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
id_find t = do 
    c <- get

    let res = catStoreFind t c 
    -- (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing) c
    return  res 

-- id_find_1st :: Node -- ^ 
--   -> StoreStateMonad [CPoint o m2]

-- id_find_1st i =    id_find  (Just . NodeTag $ i, Nothing, Nothing) 
