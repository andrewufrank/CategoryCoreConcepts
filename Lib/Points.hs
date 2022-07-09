-----------------------------------------------------------------------------
--
-- Module      :  building points with 2 coordinate values
{-  

only the point (2d) specific code
and (for now) Value Length 

op: distance between two points


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

module Lib.Points
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

-- helpers
-- type Store = CatStore ObjPoint MorphPoint
-- type StoreStateMonad = State Store  





data Point2 = Point2 Float Float  -- the data type from gloss 
-- a point in 2d (simplistic from gloss)
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
    
data Length = Length Float  
-- a distance value, should be a subobj of Value 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- data PointType c = PointType Point2 deriving (Show, Read, Ord, Eq, Generic, Zeros)

data ValueType c = Value c deriving (Show, Read, Ord, Eq, Generic, Zeros)


-- makePoint :: Int ->  Float -> Float ->   (ObjPoint, MorphPoint, ObjPoint)
-- makePoint n x y = (NodeTag (Node n), xyMorph, PointTag (Point2 x y))
-- makeEdgeTo o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))

  
-- distanceFun2   :: () =>  Node -> Node -> StoreStateMonad  (Length)     
-- distanceFun2 n1 n2 = do 
--     p1 <- xyFun n1 
--     p2 <- xyFun n2 
--     -- let d = Gloss.magV ((Gloss.(-)) (unPoint2 p1) (unPoint2 p2)) 
--     let d = mag (sub ( p1) ( p2)) 
--     return . Length $ d 
-- lengthEdge :: Edge -> StoreStateMonad (Length)
-- lengthEdge  e =   compDist <$> ( xyFun =<< sInvFun e) <*> (xyFun =<< tInvFun e) 
--     -- n1 <- sInvFun e 
--     -- n2 <- tInvFun e 
--     -- l <- distanceFun2 n1 n2 
--     -- return $ l 
--     -- f <$> x <*> y >>
-- compDist :: Point2 -> Point2 -> Length
-- compDist p1 p2 = Length .   mag $ (sub p1 p2) --(unPoint2 p1) (unPoint2 p2))  

-- -- unPoint2 :: Point2 -> Gloss.Point
-- -- unPoint2 (Point2 f) = f

-- | Trivial function for subtracting co-ordinate pairs
-- sub :: Num x => Point2 -> (x, x) -> (x, x)
sub :: Point2 -> Point2 -> Point2
sub (Point2 x1 x2) (Point2 y1 y2) = Point2 (x1 - x2) (y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
-- dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct :: Point2 -> Point2 -> Float
dotProduct (Point2 x1 x2) (Point2 y1 y2) = (x1 * x2) + (y1 * y2)

-- -- | Conversion of pair fromIntegral
-- fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
-- fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
-- mag :: Floating x => (x, x) -> x
mag :: Point2 -> Float
mag x = sqrt (dotProduct x x)

compDist :: Point2 -> Point2 -> Length
compDist p1 p2 = Length .   mag $ (sub p1 p2) --(unPoint2 p1) (unPoint2 p2))  

-- --------------------data 
-- cat0 :: CatStore ObjPoint MorphPoint
-- cat0 = catStoreEmpty
-- cat2 :: CatStore ObjPoint MorphPoint
-- cat2 = catStoreBatch (
--     [Ins (makeEdgeFrom 1 'e')
--     , Ins (makeEdgeTo   2 'e')
--     , Ins (makeEdgeFrom 2 'f')
--     , Ins (makeEdgeTo   3 'f')
--     , Ins (makePoint 1 0 0)
--     , Ins (makePoint 2 1 1)
--     ]) cat0
-- --------------- ---------------------example
-- pagePoint :: ErrIO ()
-- pagePoint = do
--     putIOwords ["\n pagePoints"]
--     -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
--     let p1 = evalState (xyFun (Node 1)) cat2
--     putIOwords ["the point for node 1", showT p1]
--     let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
--     putIOwords ["the distance 1 t 2", showT d1]
--     -- let (Edge 'e')

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