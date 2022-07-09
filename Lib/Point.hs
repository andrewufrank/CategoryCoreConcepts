-----------------------------------------------------------------------------
--
-- Module      :  building points with 2 coordinate values
{-  

two objects: Point, Coord  (Point = Coord x Coord)
two morphism: distance :: Point -> Point -> Distance 

Coord and Distance are Float (simplistic)

for computation of distance use a couple of functions locally 

 copied all the code from the preparation modules
 because I changed the name of the Obj and Morph to ..Point 

 goal is to see how this can be systematically done to avoid 
 combinatorial explosion when the number of types increases
(How to arrange for sub-x?)

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

module Lib.Point
    -- (ObjPoint (..)
    -- , MorphPoint
    -- -- , MorphPoint'(Left, Right)
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

import Vault.Triple4cat
    ( Action(..),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind), 
        getSingle1, getSingle3,
        getTarget1, getTarget3)
import Lib.Points 

----------- the category

data XY = XY 
    deriving (Show, Read, Ord, Eq, Generic)
data Distance = Distance 
    deriving (Show, Read, Ord, Eq, Generic)

data S = S  deriving (Show, Read, Ord, Eq, Generic)
data T = T  deriving (Show, Read, Ord, Eq, Generic)

-- | the morphism in the category, required for store
data MorphPoint = Stag S | Ttag T | XYtag XY | DistTag Distance | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
-- instance MorphPoint Zeros where zero = ZZm

-- constants for the tags (some have an argument, some not)
xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Distance 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T

------------------ the objects 
---- the xxTypes serve to allow further specifications
data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type Node = NodeType Int 
type Edge = EdgeType Char

------ the object sum type  with tags 

-- | the objects in the category - required for store
data ObjPoint = NodeTag (NodeType Int) | EdgeTag (EdgeType Char) | PointTag (Point2) | ValueTag (ValueType Length)  | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjPoint where zero = ZZpoint

unEdgeTag :: ObjPoint -> EdgeType Char
unEdgeTag (EdgeTag t) = t 
unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

unNodeTag :: ObjPoint -> NodeType Int
unNodeTag (NodeTag t) = t 
unNodeTag x = errorT ["unNodeTag - not a Node", showT x]
unPointTag :: ObjPoint -> Point2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unNodeTag - not a Node", showT x]
unValueTag :: ObjPoint -> ValueType Length
unValueTag (ValueTag t) = t 
unValueTag x = errorT ["unNodeTag - not a Node", showT x]


-- code depending on MonadState
type Store = CatStore ObjPoint MorphPoint
type StoreStateMonad = State Store  

-- ^ a monadic wrapper for catStoreFind applied to state
find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
        (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
find t = do 
    c <- get
    let res = catStoreFind t c 
    return  res 

-- the makes for all ...
makeEdgeFrom :: Int -> Char -> (ObjPoint, MorphPoint, ObjPoint)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeFrom o1 o2 = (NodeTag (Node o1), sMorph, EdgeTag (Edge o2))

makeEdgeTo :: Int -> Char ->   (ObjPoint, MorphPoint, ObjPoint)
makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))


makePoint :: Int ->  Float -> Float ->   (ObjPoint, MorphPoint, ObjPoint)
makePoint n x y = (NodeTag (Node n), xyMorph, PointTag (Point2 x y))
-- makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))

-- the functions and relatiosn to acess the store 

xyFun :: () =>  Node -> StoreStateMonad  (Point2)
-- ^ start with node get point (x y coordinates)
-- get the s related edge, fails on relation
xyFun i = do 
        r1  <- find  (Just . NodeTag $ i, Just xyMorph, Nothing) 
        return . unPointTag . getSingle3  $ r1

sFun :: () =>  Node -> StoreStateMonad  (Edge)
-- ^ start with node get edge
-- get the s related edge, fails on relation
sFun i = do 
        r1  <- find  (Just . NodeTag $ i, Just sMorph, Nothing) 
        return . unEdgeTag . getSingle3  $ r1
tFun :: () =>  Node -> StoreStateMonad  (Edge)
-- ^ start with node get edge using t
tFun i = do 
        r1  <- find  (Just . NodeTag $ i, Just tMorph, Nothing) 
        return . unEdgeTag . getSingle3  $ r1
sInvFun :: () =>  Edge -> StoreStateMonad  Node
-- ^ start with edge get node using s
sInvFun i = do 
        r1  <- find  (Nothing, Just sMorph, Just . EdgeTag $ i) 
        return . unNodeTag . getSingle1  $ r1
tInvFun :: () =>  Edge -> StoreStateMonad  Node
-- ^ start with edge get node using t 
tInvFun i = do 
        r1  <- find  (Nothing, Just tMorph, Just . EdgeTag $ i) 
        return . unNodeTag . getSingle1  $ r1    



lengthEdge :: Edge -> StoreStateMonad (Length)
lengthEdge  e =   compDist <$> ( xyFun =<< sInvFun e) <*> (xyFun =<< tInvFun e) 
        -- the first is a pure function, the other are all 4 monadic
 
--------------------data 
cat0 :: CatStore ObjPoint MorphPoint
cat0 = catStoreEmpty
cat2 :: CatStore ObjPoint MorphPoint
cat2 = catStoreBatch (
    [Ins (makeEdgeFrom 1 'e')
    , Ins (makeEdgeTo   2 'e')
    , Ins (makeEdgeFrom 2 'f')
    , Ins (makeEdgeTo   3 'f')
    , Ins (makePoint 1 0 0)
    , Ins (makePoint 2 1 1)
    ]) cat0
--------------- ---------------------example
pagePoint :: ErrIO ()
pagePoint = do
    putIOwords ["\n pagePoint"]
    -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
    let p1 = evalState (xyFun (Node 1)) cat2
    putIOwords ["the point for node 1", showT p1]
    -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
    -- putIOwords ["the distance 1 t 2", showT d1]
    let le = evalState (lengthEdge (Edge 'e')) cat2
    putIOwords ["the length of the edge e", showT le]

 