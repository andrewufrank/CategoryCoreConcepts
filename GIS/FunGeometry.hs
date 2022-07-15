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
    ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State



import GIS.Category
import GIS.Store
-- import GIS.Store_data

-- import GIS.Functions

-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )


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
            Just ((cost , vertex) , withoutVertex)
                | vertex == target            -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)

-- graph =
--     Map.fromList
--         [ ('a' , [(1 , 'b') , (5 , 'c')])
--         , ('b' , [(2 , 'c')])
--         , ('c' , [(1 , 'a') , (5 , 'b')])
--         ]
-- data for this graph are in cat11
-- Output:
-- Just (3,'c')

data Path a = Path {cost :: Int , trajectory :: [a]}
    deriving (Show)

instance Eq (Path a) where
    a == b = cost a == cost b

instance Ord (Path a) where
    compare a b = compare (cost a) (cost b)


-- Output:
--     Just (Path {cost = 3, trajectory = "cba"},'c')



-- try to run in state monad

shortestPathCostOnly :: Store -> (Int, Char) -> Char -> Maybe (Int, Char)
shortestPathCostOnly store startPath targetNode = 
        dijkstra step targetNode startPath 

    where
        step :: (Int , Char) -> [(Int , Char)]
        step (cost , node) =
            [ (cost + edgeCost , child)
            | (Node child, Cost edgeCost ) <- evalState (costOutgoingEdges (Node node)) store
            ]


-- shortestPathWithPathB :: MonadState (Store) m => Path Char -> m (Maybe (Path Char, Char)) 
-- shortestPathWithPathB cat11 startPath targetNode =  
--     dijkstra stepB  targetNode startPath  
    
--     where
        
-- stepB :: MonadState (Store) m => (Path Char , Char) -> m [(Path Char, Char)]
-- stepB (Path cost traj , node) =
--             [ (Path (cost + edgeCost) (child : traj) , child)
--             | (Node child, Cost edgeCost) 
--                 <- evalState (costOutgoingEdges (Node node)) cat11  
--             ]

shortestPathWithPath :: Store -> (Path Char, Char) -> Char -> Maybe (Path Char, Char)
shortestPathWithPath cat11 startPath targetNode =  
    dijkstra step  targetNode startPath  
    
    where
        step :: (Path Char , Char) -> [(Path Char, Char)]
        step (Path cost traj , node) =
            [ (Path (cost + edgeCost) (child : traj) , child)
            | (Node child, Cost edgeCost) 
                <- evalState (costOutgoingEdges (Node node)) cat11  
            ]


-- put in StateMonad 
-- not worth the troubles, but could just pass the store...
-- shortB ::  Store -> (Path Char, Char) -> Char -> Maybe (Path Char, Char)
-- shortB store   startPath targetNode = evalState (opsB startPath targetNode) store 

-- opsB startPath targetNode = do 
--     st <- get 
--     res <- shortestPathWithPathB st startPath targetNode 
--     return res


shortA ::  Store -> (Path Char, Char) -> Char -> Maybe (Path Char, Char)
shortA store   startPath targetNode = evalState (opsa startPath targetNode) store 

opsa startPath targetNode = do 
    st <- get 
    let res = shortestPathWithPath st startPath targetNode 
    return res

