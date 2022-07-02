-----------------------------------------------------------------------------
--
-- Module      :   the values for the core concept data 
{-  values for a sum type Value
-- Int, Text, ID 
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

module Vault.Value
     where -- normal prelude
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained
-- for set example code
-- import qualified Data.Set as Set
-- import Data.Set (Set)
-- import Data.Monoid
-- end 
import UniformBase 
-- import qualified Data.Map.Strict as Map
-- import Data.List (nub)
-- import GHC.Base


class Vals a where
    val :: a -> ValueSum
    unval :: ValueSum-> a   -- removed : first is proxy for unval

instance Vals Text where
    val = VT . Value 
    unval (VT (Value t)) = t
    unval z = errorT ["unval for ", showT z]

instance Vals Int where  
    val = VI . Value 
    unval (VI (Value t)) = t
    unval z = errorT ["unval for ", showT z]
    
-- instance Vals Bool where val = ValB;     unval _ (ValB t) = t
-- instance Vals B4val where val = Val4;     unval _ (Val4 t) = t
-- instance Vals Float where val = ValF;     unval _ (ValF t) = t

-- KEY - is used in Naive Triple Store !!! 

newtype Key = Key Text        deriving (Show, Read, Ord, Eq)
mkkey :: Text -> Key
mkkey = Key
unkey :: Key -> Text
unkey (Key t) = t 


--------------------------------- VALUE 
newtype Value a = Value  a
    deriving (Eq, Ord, Read, Show, Generic, Zeros)
unvalue :: Value a -> a
unvalue (Value a) = a 

type ValueF = Value Float
mkfloat :: Float -> ValueSum
mkfloat = VF . Value 
type ValueT = Value Text
mktext :: Text -> ValueSum
mktext = VT . Value
type ValueI = Value Int  -- should I select ingeger (arbitrary precision)
mkint :: Int -> ValueSum
mkint = VI . Value
type ValueO = Value Key   -- just the object id as a value (is this necessary? useful?) 
mkobjval :: Key -> ValueSum
mkobjval = VO . Value 
 

instance Functor Value where
    fmap f = Value . f . unvalue 

f1 = mkfloat 1.0
s1 = mktext "eins"

data ValueSum = VF ValueF |  VT ValueT | VI ValueI | VO ValueO | VZ
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ValueSum where zero = VZ

instance Zeros Float where zero = zero
-- add to uniformBase

newtype ID a = ID a
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
unid (ID v) = v 
type IDint = ID Int
instance Functor ID where
        fmap f = ID . f . unid
mkid :: Int -> IDint
mkid = ID
