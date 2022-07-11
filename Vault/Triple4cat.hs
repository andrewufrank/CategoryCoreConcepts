-----------------------------------------------------------------------------
--
-- Module      :  Triple storage for categories
-- uses the naive triplestore
-- 
-- a category consists of Objects (CObj) and Morphism (Morph)
-- they are properly typed for typed functions and points in the CObj

-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}

module Vault.Triple4cat 
    (
      CPoint 
    , CatStore (..), CatStores (..)
    , Action (..)  -- from NaiveTripleStore
    , isSingleton, getSingle1, getSingle3
    , getTarget3, getTarget1
    , openSingleton
    , find2fun, find2rel
    , MorphSelFun (..), MorphSelRel (..)
    ---- for tests
    -- , pageTriple4cat
    
    -- , Morph (..), Obj (..), Sobj(..), Tobj(..)
    -- , v0, v1, v2, v3, a1x, a2x
--     Vaults (..), Vault (..), VaultState
--     -- , ObjID, makeObjID
-- --    , module Vault.NaiveTriplestore
-- --    ,  Rel4Test  (..)
--     -- , B4val (..), proxyB4val, proxyFloat
--     , Val
--     , Vals (..)
--     -- ,  unVal4, isVal4
--     , Row (..)
-- --    , newNaiveStore -- for testing
    )    
    where

-- import Control.Monad.State
-- import Data.List (sort)
-- import GHC.Generics ( Generic )
import Control.Monad.State  

import UniformBase ( Generic, errorT, showT, fst3, trd3 )
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Vault.NaiveTripleStore
    ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )
-- import Vault.Value

---- helper for queries
isSingleton :: Foldable t => t a -> Bool
isSingleton a = 1 == length a
openSingleton :: Show p => [p] -> p
openSingleton [a] = a
openSingleton x = errorT ["openSingleton - not", showT x ]

getSingle1 :: (Show a, Show b, Show c) => [(a,b,c)] -> a
getSingle1 = fst3 . openSingleton
-- ^ unwrap the first if singleton 
getSingle3 :: (Show a, Show b, Show c) => [(a,b,c)] -> c
getSingle3 = trd3 . openSingleton


getTarget3 :: [(a, b, c)] -> c
-- | get target (pos3) from a singleton result 
-- just a helper
getTarget3 cps = trd3 . head  $ cps 

getTarget1 :: [(a, b, c)] -> a
-- | get target (pos 1 ) from a singleton result 
-- just a helper
getTarget1 cps = fst3 . head  $ cps 

-- ^ a monadic wrapper for catStoreFind applied to state
-- find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
--         (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
-- find t = do 
--     c <- get
--     let res = catStoreFind t c 
--     return  res 

-- | the different views of a triple - returns for Fun and Inv are different!
data MorphSelFun = Fun | Inv   
    deriving (Show, Read, Ord, Eq, Generic)
    
data MorphSelRel =  Rel | InvRel 
    deriving (Show, Read, Ord, Eq, Generic)
    

-- ^ a monadic wrapper for catStoreFind applied to state
-- applies an unwrapper
-- a relation dom -> codom 
find2rel :: (MonadState (CatStore o m) m1, Eq o, Eq m) => 
    MorphSelRel ->
    o -> 
    m ->
    (o -> codom)-> m1 [codom]
find2rel Rel s p untag = do 
    c <- get
    let res = catStoreFind (Just s, Just p, Nothing) c 
    return . map  (untag . trd3) $ res 
find2rel InvRel o p untag = do 
    c <- get
    let res = catStoreFind (Nothing, Just p, Just o) c 
    return . map  (untag . fst3) $ res 
-- find2rel _ _ _ _= errorT ["find2 can only be used for Rel and InvRel, for functions use find2fun"]

-- ^ a monadic wrapper for catStoreFind applied to state
-- applies an unwrapper and checks for single result 
-- a function dom -> codom 
-- find2fun :: (MonadState (CatStore o m2) m1, Eq o, Eq m2, Show codom) => MorphSel ->
--         ObjPoint -> 
--         MorphPoint ->
--          (CPoint o m2 -> codom)-> m1 codom
find2fun :: (MonadState (CatStore o m) m1, Show codom, Eq o, Eq m, Eq codom) =>
    MorphSelFun
    -> o
    -> m
    -> (o -> codom)
    -> m1 codom
find2fun Fun s p untag =  
    fmap openSingleton $ find2rel Rel s p untag
find2fun Inv s o untag =  fmap openSingleton $ find2rel Rel s o untag
-- find2fun_ _ _ _= errorT ["find2fun can only be used for Fun and InvFun, for relations use find2rel"]

-- --- example code 
-- data Morph = F | T  | Null  -- for storing a graph S =s,t> T 
--     deriving (Show, Read, Ord, Eq, Generic)
-- instance Zeros Morph where zero = Null

-- data Obj = SS (Sobj Int) | TT (Tobj Int) | ZZ 
--     deriving (Show, Read, Ord, Eq, Generic)
-- instance Zeros Obj where zero = ZZ

-- data Sobj a = SK a deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- data Tobj a = TK a deriving (Show, Read, Ord, Eq, Generic, Zeros)

type CPoint o m =  (o,m,o)  -- a function for a point
--             -- deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- -- type CPointGraph = CPoint Obj Morph




newtype CatStore o m = CatStoreK [CPoint o m] 
                     deriving (Show, Read, Eq)

unCatStore :: CatStore o m -> [CPoint o m]
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!

-- -- -- type CatStoreState rel = State (CatStore rel)

class CatStores o m where
    catStoreEmpty :: CatStore o m
    catStoreInsert :: CPoint o m -> CatStore o m  -> CatStore o m
    catStoreDel :: CPoint o m -> CatStore o m  -> CatStore o m 
    catStoreFind :: (Maybe o, Maybe m, Maybe o) -> CatStore o m  -> [CPoint o m]
    catStoreBatch :: [Action (o,m,o)] -> CatStore o m  -> CatStore o m 
    catStoreBatch [] ts = ts
    catStoreBatch ((Ins t) : as) ts = catStoreInsert t . catStoreBatch as $ ts
    catStoreBatch ((Del t) : as) ts = catStoreDel t . catStoreBatch as $ ts




instance (Eq o, Eq m, TripleStore o m o) => CatStores o m where
    catStoreEmpty =(CatStoreK []) :: CatStore o m
    catStoreInsert t  = wrapCatStore  (tsinsert t)  
    catStoreDel t = wrapCatStore (tsdel t) 
    catStoreFind t = tsfind t . unCatStore





