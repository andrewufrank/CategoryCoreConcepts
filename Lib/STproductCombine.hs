-----------------------------------------------------------------------------
--
-- Module      :   the combination using the triple store for the data
{- the reconstruction of the code for my 2008 paper
    combining two state graphs (business and space)
    frank08:14[TUW-166268]

-}
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
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib.STproductCombine
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
-- end 

import UniformBase 
import Lib.Rules
-- import Vault.Values
import Data.List.Extra
import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat ( Morph(T) )  
import Lib.STproductTriple
 

-- -- combined 
s0 :: (ObjST, ObjST)
s0 = (WW(WK 1), BB(BK 4))
-- type S = (W, B) -- the combined state, product
-- s0 ::  S
-- s0 = (w0, b0)

-- type P = Either V T  
-- type SP = ((ObjST, ObjST), MorphST)
type WB = (ObjST, ObjST)


-- left or right is necessary for the search (could be stored differently in tripleStore )
f0 :: ((ObjST, ObjST), MorphST) -> (ObjST, ObjST)
f0 (s, p) = morph 
                (\l -> ( w' (Tuple.fst s) (l), Tuple.snd s)) 
                (\r -> (Tuple.fst s,  b' (Tuple.snd s) (r))) p  


j :: ((w,b), MorphST' VV TT) -> MorphST' ((w,b), VV) ((w,b), TT)
j ((w,b), vt) = morph (\v -> Va ((w,b), v))
                        (\t -> Ta ((w,b),t)) vt 
-- j ((w,b), vt) = either (\v -> Va ((w,b), v))
--                         (\t -> Ta ((w,b),t)) vt 

-- distribute :: (a, Either b c) -> Either (a,b) (a, c)
-- instance Distributive (->) where
distribute :: (a, MorphST' b1 b2) -> MorphST' (a, b1) (a, b2)
distribute (a, Va b) = Va (a,b)
distribute (a, Ta c) = Ta (a,c)

 

-- -- reorganize 
-- h:: ((W,B),V) -> ((W,V),B)
h :: ((a, b1), VV) -> ((a, VV), b1)
h ((w,b),v) = ((w,v),b)
-- k::((W,B),T) -> (W,(B,T))
k :: ((a1, a2), TT) -> (a1, (a2, TT))
k ((w,b),t) = (w,(b,t))
-- apply each 
-- l' :: ((W,V),B) -> (W,B)  -- was w'
l' :: ((ObjST, VV), ObjST) -> (ObjST, ObjST)
l' wvb = cross (uncurry w',id) wvb
-- r' :: (W,(B,T)) -> (W,B)
-- r' :: (ObjST, (ObjST, TT)) -> (ObjST, ObjST)
r' :: (t1, (ObjST, TT)) -> (t1, ObjST)
r' = cross (id, uncurry b')

-- -- f1 :: ((W,B),Either V T) -> (W,B)
f1 :: (WB, MorphST) -> (ObjST, ObjST)
-- f1 :: ((ObjST, ObjST), MorphST' MorphST MorphST) -> (ObjST, ObjST)
f1 = morph (l' . h) (r' . k) . j

-- f2 :: ((W,B),Either V T) -> (W,B)
f2 = morph (cross (uncurry w', id) . h) (cross (id, uncurry b') . k) . distribute
-- f3 :: ((W,B),Either V T) -> (W,B)
f3 = morph (cross (uncurry w', id) ) (cross (id, uncurry b') ) . morph (Va . h) (Ta . k) . distribute
-- best solution
-- f4 :: ((W,B),Either V T) -> (W,B)
f4 :: ((WB),MorphST' VV TT) -> (WB)
f4 = morph (first (uncurry w') ) (second (uncurry b') ) . morph (Va . h) (Ta . k) . distribute
        -- bimap needs special code for MorphST bimap' h k 


showStates :: (((ObjST, ObjST), MorphST) -> (ObjST, ObjST)) -> Text
showStates f = showT [s0, s1, s2, s3, s4, s41, s5, s51]
    where
    s1 = f (s0, Va (VV 'a'))
    s2 = f (s1, Va (VV 'b')) --Va B)
    s3 = f (s0, Ta (TT 'c'))
    s4 = f (s1, Ta (TT 'c'))
    s5 = f (s2, Ta (TT 'c'))
    s41 = f (s3, Va (VV 'a'))
    s51 = f (s4, Va (VV 'b'))        

pageSTproductCombines :: IO ()
pageSTproductCombines = do
    putIOwords ["\npagepageSTproductCombines"]
    -- putIOwords ["combined states ", showT [s0, s1, s2, s3, s4, s41, s5, s51]]
    putIOwords ["combined states f0", showStates f0]
    putIOwords ["combined states f1", showStates f1]
    putIOwords ["combined states f2", showStates f1]
    putIOwords ["combined states f3", showStates f1]
    putIOwords ["combined states f4", showStates f1]



