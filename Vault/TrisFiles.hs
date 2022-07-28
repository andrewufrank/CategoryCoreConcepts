-----------------------------------------------------------------------------
--
-- Module      :   the core concept data 
{- two concepts only, field and objects
    focus on functions 
    but 
        no data storage
        no time or change

field: interpolation (trivial)
objects: a sigle value (no theme yet)
-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE UndecidableInstances    #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vault.TrisFiles
    (read8, write8, catStoreFileType
    , Store
    , triplePerLine
    -- for test 
    , pageTris) where 
        
        
        -- normal prelude
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained
-- for set example code
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Monoid
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
import Lib.STproductTriple
import Control.Monad.State

--  the test data (Space & Business)
type Store = CatStore ObjST MorphST
cat0 = catStoreEmpty :: Store
cat2 = catStoreBatch ([Ins (makePfeilSpace 1 'a' 2), Ins (makePfeilSpace 2 'b' 3)]) cat0
cat3 = catStoreBatch ([Ins (makePfeilBusiness 4 'c' 5)]) cat2

cat3show = "CatStoreK [(BB (BK 4),Right (TT 'c'),BB (BK 5)),(WW (WK 1),Left (VV 'a'),WW (WK 2)),(WW (WK 2),Left (VV 'b'),WW (WK 3))]"
cat3' = read cat3show :: Store

-- for typed files 
catStoreFileType :: TypedFile5 Text Store
catStoreFileType = makeTyped (Extension "tris")  :: TypedFile5 Text Store

instance TypedFiles7 Text Store where
  wrap7 = read . t2s
  unwrap7 = showT

-- | to show a list of triples as one per line
triplePerLine :: (Show a, Show b) => [[(a,b,a)]] -> Text
triplePerLine =  (unlines' .map   ( unlines'.   map showT)) 
        --hqTriple_two

-- example state

pageTris :: ErrIO ()
pageTris = do
    putIOwords ["\n vault pageTris"]
    putIOwords ["cat3\n", showT cat3]
    putIOwords ["cat3 read back \n", showT cat3']

-- show & read can be used to store the data 
    let dataFile = makeAbsFile "/home/frank/CoreConcepts/cat3"
    
    write8 dataFile catStoreFileType cat3 
    cat3'' <- read8 dataFile catStoreFileType 
    write8 (makeAbsFile "/home/frank/CoreConcepts/cat3after") catStoreFileType cat3''
                                        -- could go to test


