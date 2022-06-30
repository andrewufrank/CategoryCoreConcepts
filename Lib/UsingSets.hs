-----------------------------------------------------------------------------
--
-- Module      :   the code for aticle 1 page 13
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

module Lib.UsingSets
     where

import UniformBase 
import Lib.Page13
import Data.List (nub)
import Data.Tuple (swap)

-- instance Enum Set13 where   - is derived 
-- gives toEnum :: Int -> Set13 and 
-- fromEnum :: Set13 -> Int 

toEnum13:: Int -> Set13 
toEnum13 = toEnum

toEnum14:: Int -> Set14 
toEnum14 = toEnum

f15' :: Int -> Int 
f15' 0 = 0
f15' 1 = 3 
f15' 2 = 3 

f15'' :: Set13 -> Set14
f15'' = toEnum14 . f15' . fromEnum 

using:: IO () 
using = do 
    putIOwords ["fromEnum john mary", showT [fromEnum John, fromEnum Mary]]
    putIOwords ["toEnum 0,1", showT [toEnum13 0, toEnum13 1]]
    putIOwords ["f15'' ", showT (map f15'' [John, Mary, Sam])]

data I3 a = I3 a deriving (Eq, Ord, Show, Read)
unI3 (I3 a) = a 
toI3 :: Set13 -> I3 Int
toI3 = I3 . fromEnum 
fromI3 :: I3 Int -> Set13 
fromI3 (I3 a) = toEnum a 

instance Functor I3 where 
    fmap   f (I3 a) = I3 (f a) 


using2:: IO () 
using2 = do 
    putIOwords ["toI3", showT [map toI3 [John, Mary, Sam]]]
    putIOwords ["fromI3", showT [map fromI3[I3 0, I3 1, I3 2]]]
    putIOwords ["functor", showT (fmap id (I3 1))]

-- domain13 = dots13 -- [minBound::Set13 .. maxBound::Set13]
-- domain14 = dots14 -- [minBound  .. maxBound ]::[Set14]

-- codomain13 = domain14
-- image13 = fmap f13 dots

dots:: (Bounded a, Enum a) => [a]
dots = [minBound  .. maxBound ] 

-- dots13 = dots :: [Set13]
-- dots14 = dots :: [Set14]

injective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b)   -> Bool 
injective f = length image1 == length (nub (dots `asTypeOf` image1))
    where image1 = fmap f dots 

-- surjective = length codomain13 == length (nub image13)
surjective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b)   -> Bool 
surjective f = length codomain1 == length (nub image1)
    where   image1 = fmap f dots 
            codomain1 = fmap f dots `asTypeOf` image1

bijective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b)   -> Bool 
bijective f = injective f && surjective f  

toPfeil f a = (a, f a)
toPfeile f = map (toPfeil f) (dots)

invPfeil = map swap 

fromPfeile ((k,v):kvs) k1 = if k ==k1 then v else fromPfeile kvs k1 
fromPfeile [] k = errorT ["not a function for", showT k, "- error in inversion?"]

invf13 = fromPfeile (invPfeil $ toPfeile f13)


using3::IO ()
using3 = do 
        putIOwords ["minBound maxBound", showT [minBound::Set13, maxBound::Set13]]
        putIOwords ["all13", showT (dots ::[Set13])]
        -- putIOwords ["codomain13", showT codomain13]
        putIOwords ["injective f13", showT (injective f13)]
        putIOwords ["surjective f13", showT (surjective f13 )]
        putIOwords ["toPfeil", showT (toPfeil f13 John)]
        putIOwords ["toPfeile", showT (toPfeile f13 )]
        putIOwords ["inv toPfeile", showT (invPfeil $ toPfeile f13 )]
        putIOwords ["inv f13", showT (invf13 Eggs )]
        -- putIOwords ["inv f13 fails", showT (invf13 Toast )]
        putIOwords ["bijective f13", showT (bijective f13 )]

