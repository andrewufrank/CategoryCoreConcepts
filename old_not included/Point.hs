-----------------------------------------------------------------------------
--
-- Module      :  building points with 2 coordinate values
{-  

two objects: Point, Coord  (Point = Coord x Coord)
two morphism: distance :: Point -> Point -> Length 

Coord and Length are Double (simplistic)

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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
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
    ( zip,
      Eq,
      Ord,
      Read,
      Show,
      Char,
      Double,
      Int,
      Category((.)),
      ($),
      (<$>),
      (=<<),
      return,
      Applicative((<*>)),
      mapM )
import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State
    ( StateT, MonadState(get), evalState, execStateT, State )  
    -- ( MonadState(get), evalState, runState, State, StateT, execStateT )

-- ( MonadState(get), evalState, runState, State, StateT, execStateT )
import Vault.Triple4cat
    ( Action(Ins),
      CatStores(catStoreBatch, catStoreEmpty),
      CatStore,
      find2rel,
      find2fun,
      MorphSel (..)
    )

-- import Lib.Points ( compDist, Length, Point2(..), ValueType ) 

----------- the category

data XY = XY 
    deriving (Show, Read, Ord, Eq, Generic)
data Length = Length 
    deriving (Show, Read, Ord, Eq, Generic)

data S = S  deriving (Show, Read, Ord, Eq, Generic)
-- | the start node of an edge
data SC = SC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards s (reverse)
data T = T  deriving (Show, Read, Ord, Eq, Generic)
-- | the end node of an edge 
data TC = TC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards t (forward)

-- | the morphism in the category, required for store
data MorphPoint = Stag S | Ttag T | XYtag XY | DistTag Length 
        | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
-- instance MorphPoint Zeros where zero = ZZm

-- constants for the tags (some have an argument, some not)
xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Length 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T
scMorph = SCosttag SC
-- tcMorph = TCcosttag TC

------------------ the objects 
---- the xxTypes serve to allow further specifications
data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type Node = NodeType Char 
type Edge = EdgeType Int

-- | the cost to measure the use of a resource 
data Cost = Cost Int 
        deriving (Show, Read, Ord, Eq, Generic, Zeros)


-- ------ the object sum type  with tags 

-- -- | the objects in the category - required for store
-- -- reference to types with no parameter
-- data ObjPoint = NodeTag Node | EdgeTag Edge | PointTag (Point2) | ValueTag (ValueType Length)  | CostTag Cost | ZZpoint
--     deriving (Show, Read, Ord, Eq, Generic)
-- instance Zeros ObjPoint where zero = ZZpoint

-- unEdgeTag :: ObjPoint -> Edge 
-- unEdgeTag (EdgeTag t) = t 
-- unEdgeTag x = errorT ["unEdgeTag - not an Edge", showT x]

-- unNodeTag :: ObjPoint -> Node 
-- unNodeTag (NodeTag t) = t 
-- unNodeTag x = errorT ["unNodeTag - not a Node", showT x]
-- unPointTag :: ObjPoint -> Point2
-- unPointTag (PointTag t) = t 
-- unPointTag x = errorT ["unNodeTag - not a Node", showT x]
-- unValueTag :: ObjPoint -> ValueType Length
-- unValueTag (ValueTag t) = t 
-- unValueTag x = errorT ["unNodeTag - not a Node", showT x]
-- unCostTag (CostTag t) = t 
-- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]

-- -- code depending on MonadState
-- type Store = CatStore ObjPoint MorphPoint
-- type StoreStateMonad = State Store  
-- type StoreErrIO = StateT Store ErrIO


-- -- the makes for all ...
-- makeNodeStartingEdge :: Char -> Int -> (ObjPoint, MorphPoint, ObjPoint)
-- -- | node, edge: value to store for an s (from edge to node, the from node)
-- makeNodeStartingEdge o1 o2 = (NodeTag (Node o1), sMorph, EdgeTag (Edge o2))

-- makeNodeEndingEdge :: Char -> Int ->   (ObjPoint, MorphPoint, ObjPoint)
-- makeNodeEndingEdge o1 o2 = (NodeTag (Node o1), tMorph, EdgeTag (Edge o2))


-- makePoint :: Char ->  Double -> Double ->   (ObjPoint, MorphPoint, ObjPoint)
-- makePoint n x y = (NodeTag (Node n), xyMorph, PointTag (Point2 x y))
-- makeSCost e c = (EdgeTag (Edge e), scMorph, CostTag (Cost c))
-- -- makeTCost e c = (EdgeTag (Edge e), tcMorph, CostTag (Cost c))

-- -- the functions and relatiosn to acess the store 

-- xyFun :: () =>  Node -> StoreStateMonad  (Point2)
-- -- ^ start with node get point (x y coordinates)
-- -- get the s related edge, fails on relation
-- xyFun i = find2fun Fun (NodeTag i) xyMorph unPointTag

-- sFun :: () =>  Node -> StoreStateMonad  (Edge)
-- -- ^ start with node get edge
-- -- get the s related edge, fails on relation
-- sFun i = find2fun Fun (NodeTag i) sMorph unEdgeTag 

-- sRel :: () => Node -> StoreStateMonad [Edge]
-- sRel i = find2rel Rel (NodeTag i) sMorph unEdgeTag 

-- tFun :: () =>  Node -> StoreStateMonad  (Edge)
-- -- ^ start with node get edge using t
-- tFun i = find2fun Fun (NodeTag i) tMorph unEdgeTag 

-- sInvFun :: () =>  Edge -> StoreStateMonad  Node
-- -- ^ start with edge get node using s
-- sInvFun i = find2fun Inv (EdgeTag i) sMorph unNodeTag 

-- sInvRel :: MonadState (CatStore ObjPoint MorphPoint) m =>
--                 Edge -> m [NodeType Char]
-- sInvRel i = find2rel InvRel (EdgeTag i) sMorph unNodeTag

-- tInvFun :: () =>  Edge -> StoreStateMonad  Node
-- -- ^ start with edge get node using t 
-- tInvFun i = find2fun Inv (EdgeTag i) sMorph unNodeTag

-- sCostFun :: MonadState (CatStore ObjPoint MorphPoint) m => Edge -> m Cost
-- sCostFun i = find2fun Fun (EdgeTag i) scMorph unCostTag


-- lengthEdge :: Edge -> StoreStateMonad (Length)
-- lengthEdge  e =   compDist <$> ( xyFun =<< sInvFun e) <*> (xyFun =<< tInvFun e) 
-- --         -- the first is a pure function, the other are all 4 monadic
-- costOutgoingEdges :: Node -> StoreStateMonad [(Node, Cost)]
-- costOutgoingEdges n = do 
--         es :: [Edge] <- sRel n 
--         ns :: [Node] <- mapM tInvFun es 
--         cs :: [Cost] <- mapM sCostFun es
--         return . zip ns $ cs

-- --------------------data 

-- graph123 :: [Action (ObjPoint, MorphPoint, ObjPoint)]
-- graph123 = [Ins (makeNodeStartingEdge 'e' 1)
--     , Ins (makeNodeEndingEdge    'f' 1)
--     , Ins (makeNodeStartingEdge   'f' 2)
--     , Ins (makeNodeEndingEdge     'g' 2)
--     , Ins (makePoint 'e' 0 0)
--     , Ins (makePoint 'f' 1 1)
--     ]
-- graphShortestPathEx :: [Action (ObjPoint, MorphPoint, ObjPoint)]
-- graphShortestPathEx = 
--     [ Ins (makeNodeStartingEdge 'a' 1)
--     , Ins (makeNodeEndingEdge 'b' 1)
--     , Ins (makeNodeStartingEdge 'b' 2)
--     , Ins (makeNodeEndingEdge 'c' 2)
--     , Ins (makeNodeStartingEdge 'b' 3)
--     , Ins (makeNodeEndingEdge 'c' 3)
--     , Ins (makeNodeStartingEdge 'c' 4)
--     , Ins (makeNodeEndingEdge 'a' 4)
--     , Ins (makeNodeStartingEdge 'a' 5)
--     , Ins (makeNodeEndingEdge 'c' 5)
--     , Ins (makeSCost 1 1)
--     , Ins (makeSCost 2 2)
--     , Ins (makeSCost 3 5)
--     , Ins (makeSCost 4 1)
--     , Ins (makeSCost 5 5)
--     ]

-- cat0 :: CatStore ObjPoint MorphPoint
-- cat0 = catStoreEmpty
-- cat2 :: CatStore ObjPoint MorphPoint
-- cat2 = catStoreBatch  graph123
--      cat0
-- cat11 = catStoreBatch graphShortestPathEx cat0
-- --------------- ---------------------example
-- pagePoint :: ErrIO ()
-- pagePoint = do
--     putIOwords ["\n pagePoint"]
--     -- putIOwords ["find point from node `1", showT . xy' cat2 $ (Node 1)]
--     let p1 = evalState (xyFun (Node 'e')) cat2
--     putIOwords ["the point for node e", showT p1]
--     let p1f = evalState (xyFun (Node 'f')) cat2
--     putIOwords ["the point for node f", showT p1f]
--     -- let d1 = evalState ( distanceFun2 (Node 1) (Node 2)) cat2
--     -- putIOwords ["the distance 1 t 2", showT d1]
--     -- let le = evalState (lengthEdge (Edge 1)) cat2
--     -- putIOwords ["the length of the edge 1", showT le]

--     putIOwords ["cat11", showT cat11]
--     newcat11 <- execStateT (runWithState) cat11
--     putIOwords ["newcat11", showT newcat11]

--     -- let nc = evalState (costOutgoingEdges (Node 'a')) cat2
--     -- putIOwords ["the node-cost pairs at Node a", showT nc]


-- f op = evalState op cat11

-- runWithState :: StoreErrIO Store
-- runWithState = do 
--     putIOwords ["runWithState"]
--     let le = evalState (lengthEdge (Edge 1)) cat11
--     putIOwords ["the length of the edge 1", showT le]

--     let nc = evalState (costOutgoingEdges (Node 'a')) cat11
--     putIOwords ["the node-cost pairs at Node a", showT nc]


--     s <- get 
--     return s

