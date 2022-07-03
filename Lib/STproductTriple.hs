-----------------------------------------------------------------------------
--
-- Module      :   
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

module Lib.STproductTriple
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
import Vault.Triple4cat

data MorphST = W  | Nullst
-- w = fromPfeile [(W1,W2), (W2,W3)]
-- bb = fromPfeile [(B4,B5)]
    deriving (Show, Read, Ord, Eq, Generic)

instance Zeros MorphST where zero = Nullst

data ObjST = WW (Wobj Int) | VV (Vobj Int) | ZZst
-- the spatial part: states W and actions V 
    deriving (Show, Read, Ord, Eq)
instance Zeros ObjST where zero = ZZst

data Wobj i = WK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data Vobj i =  VK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

-- data W = W1 | W2 | W3    
--     deriving  (Show, Eq, Bounded, Enum, Ord)
-- ww :: W -> W
-- -- w = fromPfeile [(W1,W2), (W2,W3)]
-- ww W1 = W2
-- ww W2 = W3
-- ww W3 = W3

-- w0 = W1
-- w1 = ww w0
-- w2 = ww w1

-- data V = A | B 
--     deriving (Show, Read, Ord, Eq)

-- w' :: W -> V -> W
-- w' W1 A = W2
-- w' W2 B = W3
-- w' w v = errorT ["w'", showT w, showT v]

-- w11 = w' w0 A
-- w12 = w' w11 B

-- -- | the business graph
-- data B = B4 | B5    
--     deriving  (Show, Eq, Bounded, Enum, Ord)
-- bb = fromPfeile [(B4,B5)]
 
-- b0 = B4
-- b1 = bb b0

-- data T = C
--     deriving  (Show, Eq, Read, Bounded, Enum, Ord)

-- b' :: B -> T -> B
-- b' B4 C = B5
-- b' _ _ = error "b'"
-- b11 = b' b0 C 


-- -- combined 
-- type S = (W, B) -- the combined state, product
-- s0 ::  S
-- s0 = (w0, b0)

-- type P = Either V T  
-- -- f :: (S,P) -> S
-- f0 :: ((W,B),Either V T) -> (W,B)
-- -- f ((w,p),Left v) = (w' w v, p)
-- -- f ((w,p),Right t) = (w, b' p t)
-- f0 (s, p) = either 
--                 (\l -> (w' (Tuple.fst s) l, Tuple.snd s)) 
--                 (\r -> (Tuple.fst s, b' (Tuple.snd s) r)) p

-- -- f1 :: Either ((W,B),V) ((W,B),T) -> (W,B)
-- j:: ((W,B),Either V T) -> Either ((W,B),V) ((W,B),T)
-- -- distribute
-- j ((w,b), vt) = either (\v -> Left ((w,b), v))
--                         (\t -> Right ((w,b),t)) vt 

-- distribute :: (a, Either b c) -> Either (a,b) (a, c)
-- -- instance Distributive (->) where
-- distribute (a, Left b) = Left (a,b)
-- distribute (a, Right c) = Right (a,c)

-- j2:: ((W,B),Either V T) -> Either ((W,B),V) ((W,B),T)
-- -- distribute
-- j2 ((w,b), vt) = distribute ((w,b), vt)


-- -- reorganize 
-- h:: ((W,B),V) -> ((W,V),B)
-- h ((w,b),v) = ((w,v),b)
-- k::((W,B),T) -> (W,(B,T))
-- k ((w,b),t) = (w,(b,t))
-- -- apply each 
-- l' :: ((W,V),B) -> (W,B)  -- was w'
-- l' wvb = cross (uncurry w',id) wvb
-- r' :: (W,(B,T)) -> (W,B)
-- r' = cross (id, uncurry b')

-- f1 :: ((W,B),Either V T) -> (W,B)
-- f1 = either (l' . h) (r' . k) . j

-- -- | the final combination
-- -- (W,B) are the two sets of states
-- -- Either V T are the action 
-- f2 :: ((W,B),Either V T) -> (W,B)
-- f2 = either (cross (uncurry w', id) . h) (cross (id, uncurry b') . k) . distribute
-- f3 :: ((W,B),Either V T) -> (W,B)
-- f3 = either (cross (uncurry w', id) ) (cross (id, uncurry b') ) . either (Left . h) (Right . k) . distribute
-- -- best solution
-- f4 :: ((W,B),Either V T) -> (W,B)
-- f4 = either (first (uncurry w') ) (second (uncurry b') ) . bimap h k . distribute

-- f=f4
-- s1 = f (s0, (Left A))
-- s2 = f (s1, Left B)
-- s3 = f (s0, Right C)
-- s4 = f (s1, Right C)
-- s5 = f (s2, Right C)
-- s41 = f (s3, Left A)
-- s51 = f (s4, Left B)


pageST_withTriples :: IO ()
pageST_withTriples = do
    putIOwords ["\npageST_withTriples"]
    -- putIOwords ["space states ", showT [w0, w1, w2]]
    -- putIOwords ["business states ", showT [b0, b1]]
    -- putIOwords ["space states 2", showT [w0, w11, w12]]
    -- putIOwords ["business states ", showT [b0, b11]]
    -- putIOwords ["combined states ", showT [s0, s1, s2, s3, s4, s41, s5, s51]]
    -- -- putIOwords ["injective f", showT (injective f137)]
    -- -- putIOwords ["surjective f", showT (surjective f137)]
    -- -- putIOwords ["countSections f", showT (countSections f137)]
    -- -- putIOwords ["naming f ", showT (naming f137)]
    -- -- putIOwords ["stacking f ", showT (stacking f137)]
    -- -- putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
    -- -- putIOwords ["sorting f ", showT (sorting f137)]
    -- -- -- the map f137 A -> A has a codomain of the fixed points and
    -- -- -- the sorting is then done with this codomain
    -- -- putIOwords ["fixedPoints f' ", showT (fp137')]
    -- -- return ()


