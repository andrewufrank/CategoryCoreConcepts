-----------------------------------------------------------------------------
--
-- Module      :   summary of the rules as code
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib.Rules where

import Data.List (nub, group, sort)
import Data.List.Extra (groupSort )

import Data.Tuple (swap)
import UniformBase

-- see summary p.58


injective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
injective f = length image1 == length (nub (dots `asTypeOf` image1))
  where
    image1 = fmap f dots

surjective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
surjective f = length codomain1 == length (nub image1)
  where
    image1 = fmap f dots
    codomain1 =  dots `asTypeOf` image1

bijective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
bijective f = injective f && surjective f


toPfeile :: (Bounded a, Enum a) => (a->b) -> [(a,b)]
-- the arrows in the internal diagram 
toPfeile f = map (toPfeil f) (dots)
    where
    toPfeil :: (a -> b) -> a -> (a, b)
    toPfeil f1 a = (a, f1 a)

dots :: (Bounded a, Enum a) => [a]
dots = [minBound .. maxBound]

invPfeil :: [(a, b)] -> [(b, a)]
invPfeil = map swap

fromPfeile :: (Eq a, Show a) => [(a, b)] -> a -> b
fromPfeile ((k, v) : kvs) k1 = if k == k1 then v else fromPfeile kvs k1
fromPfeile [] k = errorT ["not a function for", showT k, "- error in inversion?"]

invFunct :: (Eq a, Show a, Enum a, Bounded a, Bounded p, Enum p) => (p -> a) -> a -> p
invFunct f =  if bijective f 
                then invFunct_ f
                else errorT ["not bijective"]

invFunct_ f =  fromPfeile (invPfeil $ toPfeile f)

-- section - f must be surjective (epimorphism) 
--                  one of each (from stack)

stacking :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) => (k -> v) -> [(v, [k])]
stacking fun1 = if surjective fun1 then -- groupSort (invPfeil . toPfeile $ fun1)
        groupSort $ map  (\a -> (fun1 a, a)) dots
            else []

naming :: (Bounded a, Enum a, Eq b) => (a -> b)-> [b]
naming fun1 = nub . map fun1 $ dots

countSections :: (Bounded a, Enum a, Ord b, Enum b, Bounded b) =>
             (a -> b) -> Int
countSections f -- = product . map length . map snd . stacking
        = if surjective f 
            then 
                product . map length .  stacking $ f
            else 0

    -- construct all sections - see page 93
allSections :: (Show v, Ord v, Enum v, Bounded v, Bounded k, Enum k) 
    => (k -> v) -> [(v -> k)]
allSections fun = if surjective fun 
        then map fromPfeile . seq1 $ fun
        else []
    where
    c1 :: (a, [b]) -> [(a, b)]
    c1 (a,[]) = []
    c1 (a, (b:bs)) = (a,b) : c1 (a,bs)

    -- - create stack
    sta1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) => (k -> v) -> [(v, [k])]
    sta1 fun1 = groupSort $ map  (\a -> (fun1 a, a)) dots
    -- expand
    exp1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>  (k -> v) -> [[(v,k)]]
    exp1 fun1 = map c1 (sta1 fun1)
    -- sequence all allSections, gives the functions f which are sections to g 
    seq1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>  (k -> v) -> [[(v,k)]]
    seq1 fun1 = sequence . exp1 $ fun1 


-- testSection g . f = id
testSection :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>
        (k -> v) -> (v -> k) -> Bool
testSection g f = and $ zipWith (==) (map (g.f) dots) dots


-- retraction - f must be injective (monomorphism)

fixedPoints :: (Eq a, Bounded a, Enum a) =>  (a -> a) -> [a]
fixedPoints = map fst . filter fstEqsnd . toPfeile  
    where 
        fstEqsnd (a,b) = a == b

-- sorting see page 81
sorting f = groupSort (invPfeil . toPfeile $ f)
-- sorting is a stacking of the inverse, if there is one

-- invFunct f = if bijective f then 
--                 else errorT ["not bijective"]

