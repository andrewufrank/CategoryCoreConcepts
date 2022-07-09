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

    ---- for tests
    , pageTriple4cat
    
    , Morph (..), Obj (..), Sobj(..), Tobj(..)
    , v0, v1, v2, v3, a1x, a2x
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
import UniformBase
    ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
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

--- example code 
data Morph = F | T  | Null  -- for storing a graph S =s,t> T 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Morph where zero = Null

data Obj = SS (Sobj Int) | TT (Tobj Int) | ZZ 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Obj where zero = ZZ

data Sobj a = SK a deriving (Show, Read, Ord, Eq, Generic, Zeros)
data Tobj a = TK a deriving (Show, Read, Ord, Eq, Generic, Zeros)

type CPoint o m =  (o,m,o)  -- a function for a point
            -- deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- type CPointGraph = CPoint Obj Morph


-- for test
os1 :: Obj
os1 = SS (SK 0)
os2 :: Obj
os2 = SS (SK 1)
cp1 :: (Obj, Morph, Obj)
cp1 = (os1, F, os2)
cp2 :: (Obj, Morph, Obj)
cp2 = (os2, F, SS (SK 2))

newtype CatStore o m = CatStoreK [CPoint o m] 
                     deriving (Show, Read, Eq)

unCatStore :: CatStore o m -> [CPoint o m]
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!

-- -- type CatStoreState rel = State (CatStore rel)

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


-- -------------for test 

v0 :: CatStore Obj Morph  
v0 = catStoreEmpty
v1 :: CatStore   Obj Morph
v1 = catStoreInsert cp1 v0
v2 :: CatStore  Obj Morph
v2 = catStoreInsert cp2 v1
v3 :: CatStore  Obj Morph
v3 = catStoreDel cp2 v2

a1 :: [Action (Obj, Morph, Obj)]
a1 = [Ins cp1, Ins cp2]
a1x :: CatStore Obj Morph
a1x = catStoreBatch a1 v0
a2x :: CatStore Obj Morph
a2x = catStoreBatch [Del cp2] a1x



pageTriple4cat :: IO ()
pageTriple4cat = do
    putIOwords ["\n [pageTriple4cat"]
    putIOwords ["cp1", showT cp1]
--     putIOwords ["ts one", showT x1]

    putIOwords ["CatStore empty", showT v0]
    putIOwords ["CatStore with cp1", showT v1]
    putIOwords ["CatStore added cp2, deleted cp1", showT v2]
    putIOwords ["CatStore added batch cp1 cp2", showT a1x]
    putIOwords ["CatStore  cp2", showT a2x]


