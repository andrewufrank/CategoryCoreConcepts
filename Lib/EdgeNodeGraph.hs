-----------------------------------------------------------------------------
--
-- Module      :  building edge node graph data in the triple store  
{- a graph with half-edges (preparing for quad edges in guibas stolfi triangulations

two objects: Edge Node 
two morphism: s :: T -> S and t :: T -> S (see lavwere)

store in CatStore and statemonad 

graph 1 - e - 2 - f - 3 

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
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib.EdgeNodeGraph
    -- (ObjST (..)
    -- , MorphST
    -- -- , MorphST'(Left, Right)
    -- -- , Left, Right
    -- , Wobj(..), Bobj(..)
    -- -- , either, distribute
    -- , b', w', h, k
    -- , T(..), S(..)
    -- , makePfeilSpace, makePfeilBusiness
    -- -- for test 
    -- , pageST_withTriples
    -- , pageST_withTriplesBusiness
    -- , pageSTproductCombines
    -- )    
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- end 

import UniformBase 
import Control.Monad.State

import Lib.Rules
-- import Vault.Values
import Data.List.Extra
import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat
    ( Action(..),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind) )

type MorphST  = Either (S)  (T)  
-- ^ the morphism from Node to Edge 

ss :: Either S b
ss = Left S 
tt :: Either a T
tt = Right T 


data S = S  deriving (Show, Read, Ord, Eq, Generic)
data T = T  deriving (Show, Read, Ord, Eq, Generic)

data ObjST = Node (Nobj Int) | Edge (Eobj Char) | ZZst
    deriving (Show, Read, Ord, Eq)
-- ^ the objects Node and Edge
instance Zeros ObjST where zero = ZZst


data Eobj i = EK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data Nobj i =  NK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

-- type CPointST = CPoint ObjST MorphST 

makePfeilFrom :: Int -> Char -> (ObjST, MorphST, ObjST)
-- | node, edge: value to store for an s (from edge to node, the from node)
makePfeilFrom o1 o2 = (Node (NK o1), Left (S), Edge (EK o2))

makePfeilTo :: Int -> Char ->   (ObjST, MorphST, ObjST)
makePfeilTo o1 o2 = (Node (NK o1), Right (T), Edge (EK o2))

s' :: () => CatStore ObjST MorphST -> ObjST ->   ObjST
-- | get the edge from a given node, folling the s path
-- could eventually by multiple!
s' cat ow =  getTarget3 . catStoreFind (Just ow, Just ss, Nothing) $ cat

ti' :: () => CatStore ObjST MorphST -> ObjST ->   ObjST
-- | get the node from a given edge, folling the s path
-- could eventually by multiple!
ti' cat ow =  getTarget1 . catStoreFind (Nothing, Just tt, Just ow) $ cat

getTarget3 :: [(a, b, c)] -> c
-- | get target (pos3) from a singleton result 
-- just a helper
getTarget3 cps = trd3 . head  $ cps  

getTarget1 :: [(a, b, c)] -> a
-- | get target (pos 1 ) from a singleton result 
-- just a helper
getTarget1 cps = fst3 . head  $ cps 

-- data for test 
cat0 = catStoreEmpty :: CatStore ObjST MorphST
cat2 = catStoreBatch (
    [ Ins (makePfeilFrom 1 'e')
    , Ins (makePfeilTo   2 'e')
    , Ins (makePfeilFrom 2 'f')
    , Ins (makePfeilTo   3 'f')
    ]) cat0

e1 :: ObjST
e1 = s'  cat2 $ Node (NK 1)

pageEdgeNodeGraph :: ErrIO ()
pageEdgeNodeGraph = do
    putIOwords ["\n pageEdgeNodeGraph"]
    putIOwords ["data for 1-2-3", showT cat2]
    putIOwords ["find edge starting at 1", showT . s'  cat2 $ Node (NK 1)]
    putIOwords ["e", showT e1]

    putIOwords ["find node from edge e", showT . ti'  cat2 $ e1]

    let datafn = makeAbsFile "/home/frank/CoreConcepts/edgeNode123"
    write8 datafn catStoreFileType cat2 


-- for writing to file
-- for typed files 
catStoreFileType :: TypedFile5 Text Store
catStoreFileType = makeTyped (Extension "tris")  :: TypedFile5 Text Store

instance TypedFiles7 Text Store where
  wrap7 = read . t2s
  unwrap7 = showT



-- for state monad
type StoreStateMonad = State Store  
type Store = CatStore ObjST MorphST


-- getTarget :: [(a, b1, b2)] -> b2
-- getTarget cps = trd3 . head  $ cps  

-- w'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
-- w'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat
-- b'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
-- b'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat

-- -- the functions really used
-- b' :: ObjST -> T -> ObjST
-- b' o t = b'' cat3 o (Right t)

-- w' :: ObjST -> S -> ObjST
-- w' o v =    w'' cat3 o (Left v)
--     -- state is the loaded triple store (later)

-- -- the step function 
-- f6 :: ((ObjST, ObjST), MorphST) -> (ObjST, ObjST)
-- f6 = either (cross (uncurry w', id) . h) 
--             (cross (id, uncurry b') . k)   . distribute

-- -- helpers:  reorganize 
-- -- h:: ((W,B),V) -> ((W,V),B)
-- h :: ((a, b1), S) -> ((a, S), b1)
-- h ((w,b),v) = ((w,v),b)
-- -- k::((W,B),T) -> (W,(B,T))
-- k :: ((a1, a2), T) -> (a1, (a2, T))
-- k ((w,b),t) = (w,(b,t))

-- -- distribute :: (a, MorphST' b1 b2) -> MorphST' (a, b1) (a, b2)
-- distribute (a, Left b) = Left (a,b)
-- distribute (a, Right c) = Right (a,c)

-- -- for test spatial 
-- ow1 :: ObjST
-- ow1 = Node (WK 1)
-- ow2 :: ObjST
-- ow2 = Node (WK 2)
-- mw1 = Left (S 'a' )
-- -- st1 :: (ObjST, MorphST, ObjST)
-- st1 = (ow1, mw1, ow2) 

-- st2 :: (ObjST, MorphST, ObjST)
-- -- st2 :: (ObjST, Either S T, ObjST)
-- st2 = makePfeilSpace 2 'b' 3 
-- cat0 = catStoreEmpty :: CatStore ObjST MorphST
-- cat1 :: CatStore ObjST (MorphST)
-- cat1 = catStoreInsert st1 cat0
-- cat2 :: CatStore ObjST MorphST
-- cat2 = catStoreBatch ([Ins (makePfeilSpace 1 'a' 2), Ins (makePfeilSpace 2 'b' 3)]) cat0

-- -- -- | construct function w' :: W -> V -> W 
-- f2_2 :: [CPoint ObjST MorphST]
-- f2_2 = catStoreFind (Just ow2, Just . Left $ (S 'b'), Nothing) cat2

 
-- pageST_withTriples :: IO ()
-- pageST_withTriples = do
--     putIOwords ["\npageST_withTriples"]
--     putIOwords ["space states ", showT [ow1, ow2]]
--     putIOwords ["pfeil 1 - w1 -> 2 ", showT [st1, st2]]
--     putIOwords ["empty cat store", showT cat0]
--     putIOwords [" cat store", showT cat1]
--     putIOwords [" cat store", showT cat2]
--     putIOwords [" found ow2 W2", showT f2_2]
--     putIOwords [" target", showT . getTarget $ f2_2]
--     putIOwords [" target", showT . getTarget $ f2_2]
--     putIOwords [" function w'' used ", showT $ w'' cat2 ow1 (Left $ S 'a')]
--     putIOwords [" function w'' used ", showT $ w'' cat2 ow2 (Left $ S 'b')]

-- -- for test business

-- -- -- sb1 = makePfeil 4 'c' 5
-- cat3 = catStoreBatch ([Ins (makePfeilBusiness 4 'c' 5)]) cat2


-- pageST_withTriplesBusiness :: IO ()
-- pageST_withTriplesBusiness = do
--     putIOwords ["\npageST_withTriplesBusiness"]
--     putIOwords ["space and business pfeile ", showT cat3]
--     putIOwords [" function bb'' used ", showT $ b'' cat3 (Edge(BK 4)) (Right $ T 'c')]

-- -- start state
-- s0 :: (ObjST, ObjST)
-- s0 = (Node(WK 1), Edge(BK 4))


-- showStates :: (((ObjST, ObjST), MorphST) -> (ObjST, ObjST)) -> Text
-- showStates f = showT [s0, s1, s2, s3, s4, s41, s5, s51]
--     where
--     s1 = f (s0, Left (S 'a'))
--     s2 = f (s1, Left (S 'b')) --Left B)
--     s3 = f (s0, Right (T 'c'))
--     s4 = f (s1, Right (T 'c'))
--     s5 = f (s2, Right (T 'c'))
--     s41 = f (s3, Left (S 'a'))
--     s51 = f (s4, Left (S 'b'))        

-- pageSTproductCombines :: IO ()
-- pageSTproductCombines = do
--     putIOwords ["\npagepageSTproductCombines"]
--     putIOwords ["combined states f6", showStates f6]

