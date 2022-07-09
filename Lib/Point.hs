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

-- helpers
type Store = CatStore ObjPoint MorphPoint
type StoreStateMonad = State Store  



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

xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Distance 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T

-- data ObjPoint = PointTag (PointType Text)  -- is ObjPoint in other 
-- data PointType t = PT Point 2 t deriving (Show, Read, Ord, Eq, Generic, Zeros)

data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type Node = NodeType Int 
type Edge = EdgeType Char

-- data Point2 = Point2 Float Float  -- the data type from gloss 
-- -- a point in 2d (simplistic from gloss)
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)
    
-- data Length = Length Float  
-- a distance value, should be a subobj of Value 
    -- deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data PointType c = PointType Point2 deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data ValueType c = Value c deriving (Show, Read, Ord, Eq, Generic, Zeros)

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

-- id_find :: CatStoreQ -> StoreStateMonad [CPoint ObjPoint MorphPoint]
-- ^ a monadic wrapper for catStoreFind applied to state
find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
        (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
find t = do 
    c <- get

    let res = catStoreFind t c 
    -- (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing) c
    return  res 

-- find_node_edge :: (MonadState (CatStore ObjPoint ms) StoreStateMonad, Eq ms) =>  Node -> StoreStateMonad  (CPoint ObjPoint ms)
-- -- ^ start with node get edge
-- find_node_edge i = do 
--         r1  <- find  (Just . NodeTag $ i, Nothing, Nothing) 
--         return . head $ r1

makeEdgeFrom :: Int -> Char -> (ObjPoint, MorphPoint, ObjPoint)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeEdgeFrom o1 o2 = (NodeTag (Node o1), sMorph, EdgeTag (Edge o2))

makeEdgeTo :: Int -> Char ->   (ObjPoint, MorphPoint, ObjPoint)
makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))


makePoint :: Int ->  Float -> Float ->   (ObjPoint, MorphPoint, ObjPoint)
makePoint n x y = (NodeTag (Node n), xyMorph, PointTag (Point2 x y))
-- makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))

-- xy' :: CatStore ObjPoint MorphPoint -> Node -> Point2
-- xy' cat ow =  unPointTag . getTarget3 . catStoreFind (Just . NodeTag $ ow, Just xyMorph, Nothing) $ cat

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
distanceFun2   :: () =>  Node -> Node -> StoreStateMonad  (Length)     
distanceFun2 n1 n2 = do 
    p1 <- xyFun n1 
    p2 <- xyFun n2 
    -- let d = Gloss.magV ((Gloss.(-)) (unPoint2 p1) (unPoint2 p2)) 
    let d = mag (sub ( p1) ( p2)) 
    return . Length $ d 
lengthEdge :: Edge -> StoreStateMonad (Length)
lengthEdge  e =   compDist <$> ( xyFun =<< sInvFun e) <*> (xyFun =<< tInvFun e) 
    -- n1 <- sInvFun e 
    -- n2 <- tInvFun e 
    -- l <- distanceFun2 n1 n2 
    -- return $ l 
    -- f <$> x <*> y >>
compDist :: Point2 -> Point2 -> Length
compDist p1 p2 = Length .   mag $ (sub p1 p2) --(unPoint2 p1) (unPoint2 p2))  

-- unPoint2 :: Point2 -> Gloss.Point
-- unPoint2 (Point2 f) = f

-- -- | Trivial function for subtracting co-ordinate pairs
-- -- sub :: Num x => Point2 -> (x, x) -> (x, x)
-- sub :: Point2 -> Point2 -> Point2
-- sub (Point2 x1 x2) (Point2 y1 y2) = Point2 (x1 - x2) (y1 - y2)

-- -- | Compute the sum of squares or dot product of a given pair of co-ordinates
-- -- dotProduct :: Num x => (x, x) -> (x, x) -> x
-- dotProduct :: Point2 -> Point2 -> Float
-- dotProduct (Point2 x1 x2) (Point2 y1 y2) = (x1 * x2) + (y1 * y2)

-- -- -- | Conversion of pair fromIntegral
-- -- fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
-- -- fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- -- | Compute magnitude
-- -- mag :: Floating x => (x, x) -> x
-- mag :: Point2 -> Float
-- mag x = sqrt (dotProduct x x)

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
    let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
    putIOwords ["the distance 1 t 2", showT d1]
    -- let (Edge 'e')

{-
-- | Trivial function for subtracting co-ordinate pairs
sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

-- | Conversion of pair fromIntegral
fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

-}