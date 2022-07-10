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

 
-- import           Lib.DirTree
-- import  Lib.Page13
-- import  Lib.Page39
-- import  Lib.Page86
-- -- import  Lib.Page135
-- import  Lib.Page135hask
-- import Lib.HaskGraph
-- import Lib.CCworld 
-- import Lib.CCdata
-- import Lib.ExampleSet
-- import Lib.ExampleData
-- import Lib.Rules
-- import Lib.UsingSets
-- import           Lib.OpenClass
-- import Vault.Tris
import Lib.STproduct ()
import Vault.NaiveTripleStore
import Vault.Triple4cat
import Lib.STproductTriple
-- import Lib.STproductCombine
import Lib.DataInState
import Vault.TrisFiles 
import Lib.EdgeNodeGraph
import Lib.EdgeNodeGraphOps
import Lib.EdgeNodeDistance 
import Lib.Point
import UniformBase

main :: IO ()
main =  do  -- with tests in other modules

    -- pageCCdata
    -- pageTriple4cat
    -- pageST
    -- pageST_withTriples
    -- pageST_withTriplesBusiness
    -- pageSTproductCombines
    -- pageSTproductCombines2
    -- pageNT

    startProg "CatCore" mainErrIO 

mainErrIO :: ErrIO ()
mainErrIO = do 
    -- pageTris
    -- pageDataInState
    -- pageEdgeNodeGraph
    -- part2
    -- pageEdgeNodeDistance
    pagePoint


