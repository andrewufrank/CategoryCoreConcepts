-----------------------------------------------------------------------------
--
-- Module      :   
{- storing objects in the triple store in the vault

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

module Vault.Object
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
-- end 

import UniformBase 
-- import Lib.Rules (fromPfeile)
import Vault.Value
-- import Vault.Vault
import Vault.NaiveTripleStore
import Data.List.Extra
import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple -- (snd, fst)
-- import Lib.Page13
-- import Lib.Page39

-- the spatial data as triple Store

data GraphRels = E | N | L  --  for edge node label
    deriving (Show, Read, Ord, Eq)

-- fillVault :: Vault GraphRels
-- fillVault = do   -- create a new vault and fill with data for spatial graph
--     st0 <- vaultNew
--     return st0

-- -- store0 = newNaiveStore :: Store GraphRels
-- -- store1 = ntInsert (Key 11) E (mkobjval 1) $ store0

-- fillVault =  vaultNew
-- vaultMakeObj :: [(GraphRels, Val)] -> 
--             VaultState GraphRels Key
-- vaultMakeObj pairs = do
--                 k1 <- vaultNewObjID
--                 mapM_ (\(r,v)-> vaultInsert r k1 v) pairs
--                 return k1



pageObj :: IO ()
pageObj= do
    putIOwords ["\npageObj"]
    -- putIOwords ["space states ", showT [w0, w1, w2]]
    -- putIOwords ["business states ", showT [b0, b1]]
    -- putIOwords ["space states 2", showT [w0, w11, w12]]
    -- putIOwords ["business states ", showT [b0, b11]]
    -- putIOwords ["combined states ", showT [s0, s1, s2, s3, s4, s41, s5, s51]]
    -- let st = fillVault :: Vault GraphRels
    -- putIOwords ["the vault", showT st]
    -- putIOwords ["injective f", showT (injective f137)]
    -- putIOwords ["surjective f", showT (surjective f137)]
    -- putIOwords ["countSections f", showT (countSections f137)]
    -- putIOwords ["naming f ", showT (naming f137)]
    -- putIOwords ["stacking f ", showT (stacking f137)]
    -- putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
    -- putIOwords ["sorting f ", showT (sorting f137)]
    -- -- the map f137 A -> A has a codomain of the fixed points and
    -- -- the sorting is then done with this codomain
    -- putIOwords ["fixedPoints f' ", showT (fp137')]
    -- return ()


