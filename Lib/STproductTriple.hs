-----------------------------------------------------------------------------
--
-- Module      :  building the data in the triple store  
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
    ( Action(Ins),
      CPoint,
      CatStore,
      CatStores(catStoreBatch, catStoreEmpty, catStoreInsert,
                catStoreFind) )

type MorphST  = Either (VV)  (TT)  
    -- deriving (Show, Read, Ord, Eq, Generic)
newtype VV = VV Char deriving (Show, Read, Ord, Eq, Generic)
newtype TT = TT Char deriving (Show, Read, Ord, Eq, Generic)
-- instance Zeros MorphST where zero = Nullst

data ObjST = WW (Wobj Int) | BB (Bobj Int) | ZZst
-- the spatial part: states W 
-- the business side: states B 
    deriving (Show, Read, Ord, Eq)
instance Zeros ObjST where zero = ZZst
unWW :: ObjST -> Wobj Int
unWW (WW w) = w
unBB :: ObjST -> Bobj Int
unBB (BB b) = b

data Wobj i = WK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial states W1, W2, W3
data Bobj i =  BK i deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- ^ the spatial actions (moves) A or B

type CPointST = CPoint ObjST MorphST 

makePfeilSpace :: Int -> Char -> Int -> (ObjST, Either VV b, ObjST)
makePfeilSpace o1 m o2 = (WW (WK o1), Left (VV m), WW (WK o2))

makePfeilBusiness :: Int -> Char -> Int -> (ObjST, Either a TT, ObjST)
makePfeilBusiness o1 m o2 = (BB (BK o1), Right (TT m), BB (BK o2))

getTarget :: [(a, b1, b2)] -> b2
getTarget cps = trd3 . head  $ cps  

w'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
w'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat
b'' :: () => CatStore ObjST MorphST -> ObjST -> MorphST -> ObjST
b'' cat ow pv =  getTarget . catStoreFind (Just ow, Just (pv), Nothing) $ cat

-- for test spatial 
ow1 :: ObjST
ow1 = WW (WK 1)
ow2 :: ObjST
ow2 = WW (WK 2)
mw1 = Left (VV 'a' )
-- st1 :: (ObjST, MorphST, ObjST)
st1 = (ow1, mw1, ow2) 

st2 :: (ObjST, MorphST, ObjST)
-- st2 :: (ObjST, Either VV TT, ObjST)
st2 = makePfeilSpace 2 'b' 3 
cat0 = catStoreEmpty :: CatStore ObjST MorphST
cat1 :: CatStore ObjST (MorphST)
cat1 = catStoreInsert st1 cat0
cat2 :: CatStore ObjST MorphST
cat2 = catStoreBatch ([Ins (makePfeilSpace 1 'a' 2), Ins (makePfeilSpace 2 'b' 3)]) cat0

-- -- | construct function w' :: W -> V -> W 
f2_2 :: [CPoint ObjST MorphST]
f2_2 = catStoreFind (Just ow2, Just . Left $ (VV 'b'), Nothing) cat2

 
pageST_withTriples :: IO ()
pageST_withTriples = do
    putIOwords ["\npageST_withTriples"]
    putIOwords ["space states ", showT [ow1, ow2]]
    putIOwords ["pfeil 1 - w1 -> 2 ", showT [st1, st2]]
    putIOwords ["empty cat store", showT cat0]
    putIOwords [" cat store", showT cat1]
    putIOwords [" cat store", showT cat2]
    putIOwords [" found ow2 W2", showT f2_2]
    putIOwords [" target", showT . unWW . getTarget $ f2_2]
    putIOwords [" function w'' used ", showT $ w'' cat2 ow1 (Left $ VV 'a')]
    putIOwords [" function w'' used ", showT $ w'' cat2 ow2 (Left $ VV 'b')]

-- for test business

-- -- sb1 = makePfeil 4 'c' 5
cat3 = catStoreBatch ([Ins (makePfeilBusiness 4 'c' 5)]) cat2


pageST_withTriplesBusiness :: IO ()
pageST_withTriplesBusiness = do
    putIOwords ["\npageST_withTriplesBusiness"]
    putIOwords ["space and business pfeile ", showT cat3]
    putIOwords [" function bb'' used ", showT $ b'' cat3 (BB(BK 4)) (Right $ TT 'c')]

b' :: ObjST -> MorphST -> ObjST
b' = b'' cat3
-- b'ts :: ObjST -> TT -> ObjST -- similar to STproduct
-- b'ts = b'' cat3 -- not a good idea - no unMorph function easy
--      requires different storage 
w' :: ObjST -> MorphST -> ObjST
w' = w'' cat3 

