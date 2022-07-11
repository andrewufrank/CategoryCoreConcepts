 -------------------------------------------------------------------
--
-- Module      :  just the demonstration output 
-- Copyright   :
--
-- | What can be tested here?

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Lib.Lawvere_test
    where

-- import Lib.Page13 
-- import Lib.Page39

import Lib.Rules

import UniformBase

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
import           Test.Invariant           as Rule  
import Test.QuickCheck --  (arbitraryBoundedEnum)

pageCategory :: ErrIO ()
pageCategory = do 
    putIOwords ["the ouputput from GIS.Category"]


-- instance Arbitrary Set13 where
--     arbitrary = arbitraryBoundedEnum
-- instance Arbitrary Set14 where
--     arbitrary = arbitraryBoundedEnum


-- prop_id13:: Set13 -> Bool 
-- prop_id13 a = id13 a == a 

-- prop_id14:: Set14 -> Bool 
-- prop_id14 a = id14 a == a 

-- -- prop_id13_law1 :: Set13 -> Bool 
-- -- -- (Sets13 -> Set13) -> (Set13 -> Set14) -> Set13 -> Bool 
-- -- prop_id13_law1 a = (f13 . id13) a == f13 a 

-- identity1 :: Eq b => (a -> a) -> (a -> b) -> a -> Bool 
-- identity1 idop op a = (op . idop) a == op a 
-- identity2 :: Eq b => (b -> b) -> (a -> b) -> a -> Bool 
-- identity2 idop op a = (idop . op) a == op a 

-- identity :: Eq b => (a -> a) -> (a -> b) -> (b -> b) -> a -> Bool 
-- identity ida ab idb a = identity1 ida ab a && identity2 idb ab a

-- -- prop_id13_law1 a = identity1 id13 g13 a
-- -- prop_id13_law2 a = identity2 id13 g13 a
-- -- prop_id13_law3 a = identity2 id14 f13 a
-- -- prop_id13_law4 a = identity1 id13 f13 a

-- prop_id_f13 a = identity id13 f13 id14 a 
-- prop_id_g13 a = identity id13 g13 id13 a 

-- -- from Algebra.Laws:  the identity function has a different signature. 
-- -- this is for operations on points, not for composition of functions 
-- -- prop_id13 :: Set13 -> Set13 -> Bool 
-- -- prop_id13 a b = identity id13 a b
-- -- identity :: Eq a => (a -> a -> a) -> a -> a -> Bool
-- -- identity op x y  =  leftIdentity op x y &&  rightIdentity op x y

-- -- rightIdentity :: Eq a => (a -> b -> a) -> b -> a -> Bool
-- -- rightIdentity op y x  =  x `op` y == x

-- assoc :: Eq d => (a -> b) -> (b -> c) -> (c -> d) -> a -> Bool 
-- assoc op1 op2 op3 a = (op3 . (op2 . op1)) a == ((op3 . op2) . op1) a 

-- prop_assoc1  a = assoc g13 f13 h15 a 

-- instance Arbitrary SetA where
--     arbitrary = arbitraryBoundedEnum

-- inverse :: Eq a => (a->b) -> (b->a) -> a -> Bool 
-- inverse op1 op2 a = op2 (op1 a) == a

-- prop_inverse a = inverse f (invFunct f) a