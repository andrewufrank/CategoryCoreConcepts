-----------------------------------------------------------------------------
--
-- Module      :  functions which do not rely directly on store
    -- for geometry - shortest path with (not) dijkstra

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

module GIS.FunGeometry
 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    -- ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State



import Storable.Sobj
import Storable.Store
import Storable.Retrieve
-- import GIS.Store_data

-- import GIS.Functions

-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )
import GIS.Triangulation


lengthEdge :: (MonadState (Store) m) => Edge -> m (Length)
lengthEdge  e =    compDist <$> ( xyFun =<< sFun e) <*> (xyFun =<< tFun e) 
--         -- the first is a pure function, the other are all 4 monadic
costOutgoingEdges :: MonadState (Store) m => Node -> m [(Node, Cost)]
costOutgoingEdges n = do 
        es :: [Edge] <- sInvRel n 
        ns :: [Node] <- mapM tFun es 
        cs :: [Cost] <- mapM sCostFun es
        return . zip ns $ cs

-- | Trivial function for subtracting co-ordinate pairs
-- sub :: Num x => Point2d -> (x, x) -> (x, x)
sub :: Point2d -> Point2d -> Point2d
sub (Point2d x1 y1) (Point2d x2 y2) = Point2d (x1 - x2) (y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
-- dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct :: Point2d -> Point2d -> Double
dotProduct (Point2d x1 y1) (Point2d x2 y2) = (x1 * x2) + (y1 * y2)

-- -- | Conversion of pair fromIntegral
-- fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
-- fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
-- mag :: Doubleing x => (x, x) -> x
mag :: Point2d -> Double
mag x = sqrt (dotProduct x x)

compDist :: Point2d -> Point2d -> Length
compDist p1 p2 = Length .   mag $ (sub p1 p2) --(unPoint2 p1) (unPoint2 p2))  

-- f1 op = evalState op cat11


dijkstra
    :: (Ord cost , Ord node )
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> node                               -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost1 , vertex) , withoutVertex)
                | vertex == target            -> Just (cost1 , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost1 , vertex)

-- graph =
--     Map.fromList
--         [ ('a' , [(1 , 'b') , (5 , 'c')])
--         , ('b' , [(2 , 'c')])
--         , ('c' , [(1 , 'a') , (5 , 'b')])
--         ]
-- data for this graph are in cat11
-- Output:
-- Just (3,'c')

data WayList a = WayList {cost :: Int , trajectory :: [a]}
    deriving (Show)

class WayLists a where 
    extendWay :: Int -> a -> WayList a -> WayList a  

instance WayLists a where 
    extendWay c n (WayList c1 ns) = WayList (c1 +c) (n : ns)

instance Eq (WayList a) where
    a == b = cost a == cost b

instance Ord (WayList a) where
    compare a b = compare (cost a) (cost b)

type Way = WayList Node

-- Output:
--     Just (WayList {cost = 3, trajectory = "cba"},'c')



-- try to run in state monad

-- shortestWayCostOnly :: Store -> (Int, Node) -> Node -> Maybe (Int, Node)
-- shortestWayCostOnly store startWay targetNode = 
--         dijkstra step targetNode startWay 

--     where
--         step :: (Int , Node) -> [(Int , Node)]
--         step (cost1 , node1) =
--             [ (cost1 + edgeCost , child)
--             | (Node child, Cost edgeCost ) <- evalState (costOutgoingEdges (node1)) store
--             ]


-- shortestWayWithWayB :: MonadState (Store) m => Way -> m (Maybe (Way, Node)) 
-- shortestWayWithWayB cat11 startWay targetNode =  
--     dijkstra stepB  targetNode startWay  
    
--     where
        
-- stepB :: MonadState (Store) m => (Way , Node) -> m [(Way, Node)]
-- stepB (WayList cost traj , node) =
--             [ (WayList (cost + edgeCost) (child : traj) , child)
--             | (Node child, Cost edgeCost) 
--                 <- evalState (costOutgoingEdges (Node node)) cat11  
--             ]

-- shortestWayWithWay :: Store -> (Way, Node) -> Node -> Maybe (Way, Node)
-- shortestWayWithWay cat11 startWay targetNode =  
--     dijkstra step  targetNode startWay  
    
--     where
--         -- step :: (Way , Node) -> [(Way, Node)]
--         step pl@(WayList cost1 traj , node1) =
--             [ extendWay edgeCost child pl
--                 -- (WayList (cost1 + edgeCost) (child : traj) , child)
--             | (Node child, Cost edgeCost) 
--                 <- evalState (costOutgoingEdges (node1)) cat11  
--             ]


-- put in StateMonad 
-- not worth the troubles, but could just pass the store...
-- shortB ::  Store -> (Way, Node) -> Node -> Maybe (Way, Node)
-- shortB store   startWay targetNode = evalState (opsB startWay targetNode) store 

-- opsB startWay targetNode = do 
--     st <- get 
--     res <- shortestWayWithWayB st startWay targetNode 
--     return res


-- shortA ::  Store -> (Way, Node) -> Node -> Maybe (Way, Node)
-- shortA store   startWay targetNode = evalState (opsa startWay targetNode) store 

-- opsa startWay targetNode = do 
--     st <- get 
--     let res = shortestWayWithWay st startWay targetNode 
--     return res


-- makeHQ :: Int -> (Int, Int) -> [(ObjPoint, MorphPoint, ObjPoint)]
-- makeHQ offset (s, t) = [(HQTag hqid, sMorph, NodeTag (Node (showT $ offset + s)))
--     , (HQTag hqid, sMorph, NodeTag (Node (showT $ offset + t)))]
--     where 
--         hqid = HQ $ 100 * (offset + s)  + (offset + t)

-- showT :: Node -> Node 
-- showT x = ShowT x