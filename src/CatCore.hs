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
import GIS.Category
import GIS.Store
import GIS.Functions
import GIS.FunGeometry
import GIS.Subdivisions
-- import Data.Store_data
import ExampleData.ShortestPath
import ExampleData.Triangulation

main :: IO ()
main =  do  -- with tests in other modules

    -- startProg "CatCore" mainErrIO 
    -- main2
    main3
    res <- runErr $ mainErrIO
    putIOwords ["\n--------------error in mainErrIO", showT res]
    return ()

mainErrIO :: ErrIO ()
mainErrIO = do 

    -- pageStore

    return ()
