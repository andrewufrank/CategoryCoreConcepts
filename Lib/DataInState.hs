-----------------------------------------------------------------------------
--
-- Module      :  storing data in state 
{- how to provide the data to the functions 
    cat theory deals mostly with functions with no args 

    this is a module only for testing - example

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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib.DataInState
    ( module Lib.DataInState
    )     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- end 

import UniformBase 
-- import Lib.Rules
-- import Vault.Values
-- import Data.List.Extra
import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple -- (snd, fst)
import Vault.Triple4cat
    -- ( Action(..),
    --   CPoint,
    --   CatStore,
    --   CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
    --             catStoreFind) )
-- import Lib.STproductTriple
import Control.Monad.State
import Lib.STproductTriple
import Vault.TrisFiles ( catStoreFileType, Store ) 


-- example state
type ExState = Int 
type ExMonad = State ExState

exnext :: ExState -> ExState
exnext x = 1 + x

exOne:: State ExState ExState 
exOne = do 
            i <- get 
            let j = exnext i
            put j
            return 99

pageDataInState = do
    putIOwords ["\n pageDataInState"]

-- show & read can be used to store the data 
    let dataFile = makeAbsFile "/home/frank/CoreConcepts/cat3"
    
    cat3 <- read8 dataFile catStoreFileType 
 
    putIOwords ["the cat3 file", showT cat3 ]
    putIOwords ["the sequence of evalState"]
    -- putIOwords ["evalstate ", showT $ runState del_c cat3]
    putIOwords ["runState exOne", showT $ runState (exOne) (0)]
    putIOwords ["runState id_c", showT $ runState (id_c) (cat3)]
    putIOwords ["runState id_batch d_c", showT $ runState (id_batch d_c) (cat3)]
    putIOwords ["runState id_batch d_c", showT $ runState (id_op1) (cat3)]
    putIOwords ["runState id_find", showT $ runState (id_find (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing)) (cat3)]
    putIOwords ["evalState id_find wk2", showT $ evalState (id_find_1st 2) (cat3)]

type StoreStateMonad = State Store  

type STactions = [Action (ObjST, MorphST, ObjST)]

d_c :: STactions
d_c = [Del (makePfeilBusiness 4 'c' 5)]   :: STactions
d_a = [Del (makePfeilSpace 1 'a' 2)]   :: STactions

id_c :: StoreStateMonad Int 
-- ^ do nothing in monad (for test)
id_c = do 
    return 0 

id_batch :: STactions -> StoreStateMonad Int 
-- ^ apply the actions in the batch 
id_batch a = do 
    c <- get 
    let res = catStoreBatch a c
    put res
    return 0 

type CatStoreQuery o m = (Maybe o, Maybe m, Maybe o)
type CatStoreQ = CatStoreQuery ObjST MorphST
id_find :: CatStoreQ -> StoreStateMonad [CPoint ObjST MorphST]
id_find t = do 
    c <- get

    let res = catStoreFind t c 
    -- (Just $ WW (WK 2), Just . Left $ (VV 'b'), Nothing) c
    return res 

id_find_1st i = id_find (Just . WW. WK $ i, Nothing, Nothing) 

id_op1 :: StoreStateMonad Int
id_op1 = do 
    id_batch d_c 
    id_batch d_a
    return 0

-- init_cat cc = do 
--     state (zero, cc)
--     return ()

-- del_c = do 
--     c3 <- get 
--     let 
--         c2 = catStoreBatch ([Del (makePfeilBusiness 4 'c' 5)]) c3 
--     put c2 
--     return () 

-- inc



pageDataInState :: ErrIO ()


                                        -- could go to test




-- type MorphST  = Either (VV)  (TT)  
-- -- data MorphST' v t = Left v | Right t| ZZact
-- --     deriving (Show, Read, Ord, Eq, Generic)
-- -- type MorphST = MorphST' VV TT

-- newtype VV = VV Char deriving (Show, Read, Ord, Eq, Generic)
-- newtype TT = TT Char deriving (Show, Read, Ord, Eq, Generic)

-- data ObjST = WW (Wobj Int) | BB (Bobj Int) | ZZst
--     deriving (Show, Read, Ord, Eq)
-- -- ^ the spatial part: states W 
-- -- ^ the business side: states B 
-- instance Zeros ObjST where zero = ZZst


-- data Wobj i = WK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- -- ^ the spatial states W1, W2, W3
-- data Bobj i =  BK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- -- ^ the spatial actions (moves) A or B

-- -- type CPointST = CPoint ObjST MorphST 

-- makePfeilSpace :: Int -> Char -> Int -> (ObjST, MorphST, ObjST)
-- makePfeilSpace o1 m o2 = (WW (WK o1), Left (VV m), WW (WK o2))

-- makePfeilBusiness :: Int -> Char -> Int -> (ObjST, MorphST, ObjST)
-- makePfeilBusiness o1 m o2 = (BB (BK o1), Right (TT m), BB (BK o2))

-- getTarget :: [(a, b1, b2)] -> b2
-- getTarget cps = trd3 . head  $ cps  

-- w'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
-- w'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat
-- b'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
-- b'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat

-- -- the functions really used
-- b' :: ObjST -> TT -> ObjST
-- b' o t = b'' cat3 o (Right t)

-- w' :: ObjST -> VV -> ObjST
-- w' o v =    w'' cat3 o (Left v)
--     -- state is the loaded triple store (later)

-- -- the step function 
-- f6 :: ((ObjST, ObjST), MorphST) -> (ObjST, ObjST)
-- f6 = either (cross (uncurry w', id) . h) 
--             (cross (id, uncurry b') . k)   . distribute

-- -- helpers:  reorganize 
-- -- h:: ((W,B),V) -> ((W,V),B)
-- h :: ((a, b1), VV) -> ((a, VV), b1)
-- h ((w,b),v) = ((w,v),b)
-- -- k::((W,B),T) -> (W,(B,T))
-- k :: ((a1, a2), TT) -> (a1, (a2, TT))
-- k ((w,b),t) = (w,(b,t))

-- -- distribute :: (a, MorphST' b1 b2) -> MorphST' (a, b1) (a, b2)
-- distribute (a, Left b) = Left (a,b)
-- distribute (a, Right c) = Right (a,c)

-- -- for test spatial 
-- ow1 :: ObjST
-- ow1 = WW (WK 1)
-- ow2 :: ObjST
-- ow2 = WW (WK 2)
-- mw1 = Left (VV 'a' )
-- -- st1 :: (ObjST, MorphST, ObjST)
-- st1 = (ow1, mw1, ow2) 

-- st2 :: (ObjST, MorphST, ObjST)
-- -- st2 :: (ObjST, Either VV TT, ObjST)
-- st2 = makePfeilSpace 2 'b' 3 
-- cat0 = catStoreEmpty :: CatStore ObjST MorphST
-- cat1 :: CatStore ObjST (MorphST)
-- cat1 = catStoreInsert st1 cat0
-- cat2 :: CatStore ObjST MorphST
-- cat2 = catStoreBatch ([Ins (makePfeilSpace 1 'a' 2), Ins (makePfeilSpace 2 'b' 3)]) cat0

-- -- -- | construct function w' :: W -> V -> W 
-- f2_2 :: [CPoint ObjST MorphST]
-- f2_2 = catStoreFind (Just ow2, Just . Left $ (VV 'b'), Nothing) cat2

 
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
--     putIOwords [" function w'' used ", showT $ w'' cat2 ow1 (Left $ VV 'a')]
--     putIOwords [" function w'' used ", showT $ w'' cat2 ow2 (Left $ VV 'b')]

-- -- for test business

-- -- -- sb1 = makePfeil 4 'c' 5
-- cat3 = catStoreBatch ([Ins (makePfeilBusiness 4 'c' 5)]) cat2


-- pageST_withTriplesBusiness :: IO ()
-- pageST_withTriplesBusiness = do
--     putIOwords ["\npageST_withTriplesBusiness"]
--     putIOwords ["space and business pfeile ", showT cat3]
--     putIOwords [" function bb'' used ", showT $ b'' cat3 (BB(BK 4)) (Right $ TT 'c')]

-- -- start state
-- s0 :: (ObjST, ObjST)
-- s0 = (WW(WK 1), BB(BK 4))


-- showStates :: (((ObjST, ObjST), MorphST) -> (ObjST, ObjST)) -> Text
-- showStates f = showT [s0, s1, s2, s3, s4, s41, s5, s51]
--     where
--     s1 = f (s0, Left (VV 'a'))
--     s2 = f (s1, Left (VV 'b')) --Left B)
--     s3 = f (s0, Right (TT 'c'))
--     s4 = f (s1, Right (TT 'c'))
--     s5 = f (s2, Right (TT 'c'))
--     s41 = f (s3, Left (VV 'a'))
--     s51 = f (s4, Left (VV 'b'))        

-- pageSTproductCombines :: IO ()
-- pageSTproductCombines = do
--     putIOwords ["\npagepageSTproductCombines"]
--     putIOwords ["combined states f6", showStates f6]

