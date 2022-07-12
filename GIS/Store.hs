-----------------------------------------------------------------------------
--
-- Module      :  the store for the category in GIS.Category
{-  

 construct the two sum types and the storage
 
 the morphis here?
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GIS.Store
 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State

import Vault.Triple4cat
    ( Action(Ins),
      CatStores(catStoreBatch, catStoreEmpty),
      CatStore,
      find2rel,
      find2fun,
      MorphSel(Forward, Inv) )

import GIS.Category


----------- the category


-- | the morphism in the category, required for store
data MorphPoint = Stag S | Ttag T | XYtag XY | DistTag Distance 
        | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        | ZZm 
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
scMorph = SCosttag SC
-- tcMorph = TCcosttag TC

------------------ the objects 
------ the object sum type  with tags 

-- | the objects in the category - required for store
-- reference to types with no parameter
data ObjPoint = NodeTag Node | EdgeTag Edge | PointTag (Point2) | ValueTag (ValueType Length)  | CostTag Cost | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjPoint where zero = ZZpoint

unEdgeTag :: ObjPoint -> Edge 
unEdgeTag (EdgeTag t) = t 
unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

unNodeTag :: ObjPoint -> Node 
unNodeTag (NodeTag t) = t 
unNodeTag x = errorT ["unNodeTag - not a Node", showT x]
unPointTag :: ObjPoint -> Point2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unNodeTag - not a Node", showT x]
unValueTag :: ObjPoint -> ValueType Length
unValueTag (ValueTag t) = t 
unValueTag x = errorT ["unNodeTag - not a Node", showT x]
unCostTag (CostTag t) = t 
unCostTag x = errorT ["unCostTag -  not a Cost", showT x]

-- code depending on MonadState
type Store = CatStore ObjPoint MorphPoint
type StoreStateMonad = State Store  
type StoreErrIO = StateT Store ErrIO


-- the makes for all ...
makeNodeStartingEdge :: Char -> Int -> (ObjPoint, MorphPoint, ObjPoint)
-- | node, edge: value to store for an s (from edge to node, the from node)
makeNodeStartingEdge o1 o2 = (NodeTag (Node o1), sMorph, EdgeTag (Edge o2))

makeNodeEndingEdge :: Char -> Int ->   (ObjPoint, MorphPoint, ObjPoint)
makeNodeEndingEdge o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))


makePoint :: Char ->  Float -> Float ->   (ObjPoint, MorphPoint, ObjPoint)
makePoint n x y = (NodeTag (Node n), xyMorph, PointTag (Point2 x y))
makeSCost e c = (EdgeTag (Edge e), scMorph, CostTag (Cost c))
-- makeTCost e c = (EdgeTag (Edge e), tcMorph, CostTag (Cost c))

-- the functions and relatiosn to acess the store 

xyFun :: (MonadState (Store) m) =>  Node -> m  (Point2)
-- ^ start with node get point (x y coordinates)
xyFun i = find2fun Forward (NodeTag i) xyMorph unPointTag

sFun :: (MonadState (Store) m) =>  Node -> m  (Edge)
-- ^ start with node get edge
sFun i = find2fun Forward (NodeTag i) sMorph unEdgeTag 

sRel :: (MonadState (Store) m) => Node -> m [Edge]
sRel i = find2rel Forward (NodeTag i) sMorph unEdgeTag 

tFun :: (MonadState (Store) m) =>  Node -> m  (Edge)
-- ^ start with node get edge using t
tFun i = find2fun Forward (NodeTag i) tMorph unEdgeTag 

tRel :: (MonadState (Store) m) =>  Node -> m  [Edge]
-- ^ start with node get edge using t
tRel i = find2rel Forward (NodeTag i) tMorph unEdgeTag 

sInv :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with edge get node using s
sInv i = find2fun Inv (EdgeTag i) sMorph unNodeTag 

sInvRel :: MonadState (Store) m =>  Edge -> m [Node]
sInvRel i = find2rel Inv (EdgeTag i) sMorph unNodeTag

tInv :: (MonadState (Store) m) =>  Edge -> m  Node
-- ^ start with edge get node using t 
tInv i = find2fun Inv (EdgeTag i) tMorph unNodeTag

tInvRel :: (MonadState (Store) m) =>  Edge -> m  [Node]
-- ^ start with edge get node using t 
tInvRel i = find2rel Inv (EdgeTag i) tMorph unNodeTag

sCostFun :: MonadState (Store) m => Edge -> m Cost
sCostFun i = find2fun Forward (EdgeTag i) scMorph unCostTag


lengthEdge :: (MonadState (Store) m) => Edge -> m (Length)
lengthEdge  e =    compDist <$> ( xyFun =<< sInv e) <*> (xyFun =<< tInv e) 
--         -- the first is a pure function, the other are all 4 monadic
costOutgoingEdges :: MonadState (Store) m => Node -> m [(Node, Cost)]
costOutgoingEdges n = do 
        es :: [Edge] <- sRel n 
        ns :: [Node] <- mapM tInv es 
        cs :: [Cost] <- mapM sCostFun es
        return . zip ns $ cs

-- | Trivial function for subtracting co-ordinate pairs
-- sub :: Num x => Point2 -> (x, x) -> (x, x)
sub :: Point2 -> Point2 -> Point2
sub (Point2 x1 y1) (Point2 x2 y2) = Point2 (x1 - x2) (y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
-- dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct :: Point2 -> Point2 -> Float
dotProduct (Point2 x1 y1) (Point2 x2 y2) = (x1 * x2) + (y1 * y2)

-- -- | Conversion of pair fromIntegral
-- fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
-- fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
-- mag :: Floating x => (x, x) -> x
mag :: Point2 -> Float
mag x = sqrt (dotProduct x x)

compDist :: Point2 -> Point2 -> Length
compDist p1 p2 = Length .   mag $ (sub p1 p2) --(unPoint2 p1) (unPoint2 p2))  

-- f1 op = evalState op cat11

