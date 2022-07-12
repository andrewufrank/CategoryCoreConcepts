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
import GIS.Store_data
import GIS.Functions
-- import GIS.FunGeometry

main :: IO ()
main =  do  -- with tests in other modules

    -- startProg "CatCore" mainErrIO 
    main2

mainErrIO :: ErrIO ()
mainErrIO = do 

    pageStore


