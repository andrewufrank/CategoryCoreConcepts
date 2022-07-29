-----------------------------------------------------------------------------
--
-- Module      :   the main for the core concepts data  
------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main     where      -- must have Main (main) or Main where

import UniformBase
-- import GIS.Category
import Storable.Store
import GIS.Functions
import GIS.FunGeometry
-- import GIS.Triangulation
-- import Data.Store_data
import ExampleData.DelaunayData
import Storable.MakesTess

-- import ExampleData.ShortestPathData
-- import ExampleData.TriangulationData

main :: IO ()
main =  do  -- with tests in other modules

    -- startProg "CatCore" mainErrIO 
    -- main1
    -- main2
    -- main3  -- triangulation
    res <- runErr $ mainErrIO
    putIOwords ["\n--------------error in mainErrIO", showT res]
    return ()

mainErrIO :: ErrIO ()
mainErrIO = do 

    -- pageStore -- from ShortestPath
    -- runWithState
    mainDel
    mainMakeTess
    return ()
