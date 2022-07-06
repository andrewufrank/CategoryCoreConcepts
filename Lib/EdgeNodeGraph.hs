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
    ( MonadState(get), evalState, runState, State )

-- import Lib.Rules
-- import Vault.Values
-- import Data.List.Extra
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
cat0 :: CatStore ObjST MorphST
cat0 = catStoreEmpty :: CatStore ObjST MorphST
cat2 :: CatStore ObjST MorphST
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

    putIOwords ["runState id_find", showT $ runState (id_find (Just $ Node . NK $ 1, Just ss, Nothing) ) (cat2)]
    putIOwords ["evalState id_find wk2", showT $ evalState (id_find_1st 1) (cat2)]


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

type CatStoreQuery o m = (Maybe o, Maybe m, Maybe o)
type CatStoreQ = CatStoreQuery ObjST MorphST

id_find :: CatStoreQ -> StoreStateMonad [CPoint ObjST MorphST]
-- ^ a monadic wrapper for catStoreFind applied to state
id_find t = do 
    c <- get

    let res = catStoreFind t c 
    -- (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing) c
    return res 

id_find_1st :: Int -> StoreStateMonad [CPoint ObjST MorphST]
id_find_1st i = id_find (Just . Node . NK $ i, Nothing, Nothing) 
