-----------------------------------------------------------------------------
--
-- Module      :  all the example data to import
-- page 13: sets, maps, composition
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

module Lib.ExampleData 
     where

import UniformBase 
import Lib.Rules

--------- page 39 

-- the objects
data SetA = Mother | Father | Child deriving (Show, Eq, Bounded, Enum, Ord)
data SetB = Feather | Stone | Flower  deriving (Show, Eq, Ord, Bounded, Enum)
 
-- the maps 
f39 :: SetA -> SetB
f39 (Mother) = Feather
f39 (Father) = Stone
f39 (Child) = Flower

-- exercise 5 - page 91
data A5 = A5b | A5p | A5q | A5r | A5s deriving (Show, Eq, Bounded, Enum, Ord)
data B5 = B50 | B51 deriving (Show, Eq, Bounded, Enum, Ord)

g39 :: A5 -> B5
g39 = fromPfeile [(A5b,B50), (A5p,B50), (A5q,B50), (A5r,B51),
     (A5s,B51)]

page39data:: IO ()
page39data = do
    putIOwords ["\npage39data:\n"]
    putIOwords ["injective f39", showT (injective f39)]
    putIOwords ["surjective f39", showT (surjective f39)]
    putIOwords ["bijective f39", showT (bijective f39)]
    putIOwords ["countSections", showT $ countSections f39]
    putIOwords ["stacking f13", showT.stacking  $  f13]
    putIOwords ["naming f13", showT.naming  $  f13]
    return ()


-------- page 13 
-- the objects
data Set13 = John | Mary | Sam deriving (Show, Eq, Ord, Bounded, Enum)
data Set14 = Eggs | Toast | Oatmeal | Coffee deriving (Show, Eq, Ord, Bounded, Enum)
data Set15 = D1 | D2 | D3 | D4 | D5 | D6 deriving (Show, Eq, Bounded, Enum)
-- the maps 
f13 :: Set13 -> Set14
f13 (John) = Eggs
f13 (Mary) = Coffee
f13 (Sam) = Coffee

g13 :: Set13 -> Set13
g13(John) = Mary
g13(Sam) = Mary
g13(Mary) = John 

h15 :: Set14 -> Set15 
h15(Eggs) = D1 
h15(Coffee)= D2 
h15(Toast) = D4 
h15(Oatmeal) = D5

id13:: Set13 -> Set13 -- identity map 
id13 a = a 
id14:: Set14 -> Set14
id14 a = a 


-- points 
data Set1 = One deriving (Show, Eq, Bounded, Enum)
john, mary, sam :: Set1 -> Set13
john One = John 
mary One = Mary
sam One = Sam 

page13data :: IO ()
page13data = do
    putStrLn "\nLib.Page13data:\n"
    putIOwords ["injective f13", showT (injective f13)]
    putIOwords ["surjective f13", showT (surjective f13)]
    putIOwords ["bijective f13", showT (bijective f13)]
    return ()





