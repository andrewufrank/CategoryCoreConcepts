-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}  
                 -- htf_main_thisModulesTests is not used
 

-- module Spec     where      -- must have Main (main) or Main where

import           Test.Framework
-- import  {-@ HTF_TESTS @-}         Lib.Lawvere_test
import  {-@ HTF_TESTS @-}         Vault.NaiveTripleStore_test
import  {-@ HTF_TESTS @-}         Vault.Triple4cat_test

main :: IO ()
main =  do
    pageTriple4cat
    putStrLn "Lib.Testing.hs for Lawvere_test:\n"
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()


