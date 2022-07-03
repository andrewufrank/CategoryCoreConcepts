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
    -- (
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
    -- )    
    where

-- import Control.Monad.State
-- import Data.List (sort)
import GHC.Generics
import UniformBase
import Vault.NaiveTripleStore
-- import Vault.Value

data Morph = F | T  | Null  -- for storing a graph S =s,t> T 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Morph where zero = Null

data Obj = SS (Sobj Int) | TT (Tobj Int) | ZZ 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Obj where zero = ZZ

data Sobj a = SK a deriving (Show, Read, Ord, Eq, Generic, Zeros)
data Tobj a = TK a deriving (Show, Read, Ord, Eq, Generic, Zeros)

type CPoint =  (Obj, Morph, Obj)  -- a function for a point
            -- deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- for test
os1 :: Obj
os1 = SS (SK 0)
os2 :: Obj
os2 = SS (SK 1)
cp1 :: (Obj, Morph, Obj)
cp1 = (os1, F, os2)

newtype CatStore = CatStoreK [CPoint] 
                     deriving (Show, Read, Eq)

unCatStore :: CatStore -> [CPoint]
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint] -> [CPoint]) -> CatStore -> CatStore
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!

-- -- type CatStoreState rel = State (CatStore rel)

-- class CatStores rel where
--     CatStoreEmpty :: CatStore
--     CatStoreInsert :: CPoint -> CatStore  -> CatStore 
--     CatStoreDel :: CPoint -> CatStore  -> CatStore 

-- --
-- data GraphRels = Edge | Node | Label  --  for edge node label
--     deriving (Show, Read, Ord, Eq)

-- instance () => CatStores (GraphRels) where
--     CatStoreEmpty =( CatStoreK []) :: CatStore GraphRels
--     -- CatStoreInsert t  = CatStoreK .  tsinsert t   . unCatStore
--     CatStoreInsert t  = wrapCatStore  (tsinsert t)  
--     CatStoreDel t = wrapCatStore (tsdel t)  

-- -- t2v :: TripleCC GraphRels -> CatStoreK (TripleCC GraphRels) 
-- -- t2v a = CatStoreK a1
-- --     where a1 = a :: TripleCC GraphRels 

-- -------------for test 
-- e1 :: TripleCC GraphRels  -- (Key, GraphRels, ValueSum)
-- e1 = (mkkey "x1", Edge, mktext "label x1")
-- e2 :: (Key, GraphRels, ValueSum)
-- e2 = (mkkey "x2", Edge, mktext "label x2")
-- x0 :: [TripleCC GraphRels]
-- x0 = tsempty 
-- x1 :: [TripleCC GraphRels]
-- x1 = tsinsert  e1 x0

-- v0 :: CatStore  
-- v0 = CatStoreEmpty
-- v1 :: CatStore  
-- v1 = CatStoreInsert e1 v0
-- v2 :: CatStore GraphRels
-- v2 = CatStoreInsert e2 v1
-- v3 :: CatStore GraphRels
-- v3 = CatStoreDel e2 v2

pageTriple4cat :: IO ()
pageTriple4cat = do
    putIOwords ["\n [pageTriple4cat"]
    putIOwords ["cp1", showT cp1]
--     putIOwords ["ts one", showT x1]

    -- putIOwords ["CatStore empty", showT v0]
--     putIOwords ["CatStore with e1", showT v1]
--     putIOwords ["CatStore deleted e1", showT v2]


