-----------------------------------------------------------------------------
--
-- Module      :  the data for the delaunay examples
{-  

 only the data and the output
 but initially used to use the functionality from uniform-geometry
-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant return" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module ExampleData.DelaunayData
 
     where


import UniformBase
--     ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
import Control.Monad.State


import Vault.Triple4cat
import Vault.TrisFiles
-- import GIS.Category
import Storable.Store  
-- import GIS.Functions
import GIS.FunGeometry
-- import GIS.Triangulation
import Storable.Makes
import Storable.Retrieve

import Uniform.DelaunayTriples  hiding (HqID(..))
    -- should export all what is needed 

mainDel :: ErrIO () 
mainDel = do 
    putIOwords ["----------------------- delaunay data"]

    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    res4 <- liftIO $ delaunay2 fourV2
    let hqfaces = trip_surface2 400 res4
    putIOwords ["\nfaces res4\n", showT hqfaces]

    let hqnxy = trip_xy N 400 (vertices res4)
    -- let hqny = trip_y N 400 (vertices res4)
    putIOwords ["\nnode hqs for res4\n", showT hqnxy]
    -- let hqfx = trip_x F 400 (center res4)
    let hqfxy = trip_xy F 400 (center res4)
    putIOwords ["\ncenter hqs for res4\n", showT hqfxy]

    let hqlength = trip_hq_lengthX 400 res4
    let hqlength2 = trip_hq_length2X 400 res4
    putIOwords ["\nhqs length for res4\n", showT hqlength, showT hqlength2]

    let hqs = trip_hqs_faces 400 res4
    putIOwords ["\nall the face hqs for res4\n", showT hqs]
----



    -- putIOwords ["res4x - produced from fourV in Cat..", showT res4x]
    -- let hqs = trip_hqs_faces 400 res4x
    -- -- putIOwords ["\nall the face hqs for res4x\n", triplePerLine hqs]
    -- putIOwords ["\nall the face hqs for res4\n", showT hqs]

--------------------data 

-- runWithState :: StoreErrIO ()
-- runWithState = do 
--     putIOwords ["runWithState"]

--     -- catxx <- get 
--     -- putIOwords ["cat", showT catxx]
--     -- let n1 = evalState (sRel (Node "a")) cat11 -- > [Edge 1,Edge 5]
--     -- putIOwords ["sRel von Node a", showT n1]
--     n1 <- sInvRel (Node 1) -- > [Edge 1,Edge 5]
--     putIOwords ["sRel von Node a", showT n1]
--     e1 <- sFun (Edge 1)  -- > (Node a)
--     putIOwords ["sInv from Edge 1", showT e1]
--     p1 <- xyFun (Node 1)
--     putIOwords ["the xy of Edge 1", showT p1]
--     le <- lengthEdge (Edge 1) 
--     putIOwords ["the length of the edge 1", showT le]

--     let nc = evalState (costOutgoingEdges (Node 1)) cat11
--     putIOwords ["the node-cost pairs at Node a", showT nc]


--     -- s <- get 
--     return ()

-- -- to test shortest path function 

-- -- main1 = print $ dijkstra step "c" (0 , "a")
-- --     where
-- --         step :: (Int , Node) -> [(Int , Node)]
-- --         step (cost1 , node1) =
-- --             [ (cost1 + edgeCost , child)
-- --             | (Node child, Cost edgeCost ) <- evalState (costOutgoingEdges (Node node1))cat11
-- --                     ]
-- -- -- replace Map.lookup node with costOutgoingEdges
-- -- -- evalState (costOutgoingEdges node) cat11
-- -- -- removed fromMaybe []  (because find returns [])
-- -- -- and flip result
