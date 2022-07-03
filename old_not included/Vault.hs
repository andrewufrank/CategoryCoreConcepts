-----------------------------------------------------------------------------
--
-- Module      :  VaultState storage
-- uses the naive triplestore
-- and wraps in state monad
-- the subject (are restricted to Key text) and preicates (GraphRels)
-- 
-- not useful for categories (or probably anything similar)
-- abandoned
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

module Vault.Vault 
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
import UniformBase
import Vault.NaiveTripleStore
import Vault.Value

type TripleCC a =  (Key, a, ValueSum) 
            -- deriving (Show, Read, Ord, Eq, Generic, Zeros)
newtype Vault  a = VaultK [TripleCC a] 
                     deriving (Show, Read, Eq)

unVault :: Vault a -> [TripleCC a]
unVault (VaultK as) = as
wrapVault :: ([TripleCC a] -> [TripleCC a]) -> Vault a -> Vault a
wrapVault f = VaultK . f . unVault  -- not a functor!

-- type VaultState rel = State (Vault rel)

class Vaults rel where
    vaultEmpty :: Vault rel
    vaultInsert :: TripleCC rel -> Vault rel -> Vault rel
    vaultDel :: TripleCC rel -> Vault rel -> Vault rel

--
data GraphRels = Edge | Node | Label  --  for edge node label
    deriving (Show, Read, Ord, Eq)

instance () => Vaults (GraphRels) where
    vaultEmpty =( VaultK []) :: Vault GraphRels
    -- vaultInsert t  = VaultK .  tsinsert t   . unVault
    vaultInsert t  = wrapVault  (tsinsert t)  
    vaultDel t = wrapVault (tsdel t)  

-- t2v :: TripleCC GraphRels -> VaultK (TripleCC GraphRels) 
-- t2v a = VaultK a1
--     where a1 = a :: TripleCC GraphRels 

-------------for test 
e1 :: TripleCC GraphRels  -- (Key, GraphRels, ValueSum)
e1 = (mkkey "x1", Edge, mktext "label x1")
e2 :: (Key, GraphRels, ValueSum)
e2 = (mkkey "x2", Edge, mktext "label x2")
x0 :: [TripleCC GraphRels]
x0 = tsempty 
x1 :: [TripleCC GraphRels]
x1 = tsinsert  e1 x0

v0 :: Vault ( GraphRels)
v0 = vaultEmpty
v1 :: Vault (GraphRels)
v1 = vaultInsert e1 v0
v2 :: Vault GraphRels
v2 = vaultInsert e2 v1
v3 :: Vault GraphRels
v3 = vaultDel e2 v2

pageVault :: IO ()
pageVault = do
    putIOwords ["\n [pageVault"]
    putIOwords ["ts empty", showT x0]
    putIOwords ["ts one", showT x1]

    putIOwords ["vault empty", showT v0]
    putIOwords ["vault with e1", showT v1]
    putIOwords ["vault deleted e1", showT v2]


    -------------old
--     vaultInsert :: () => rel -> Key -> Val -> VaultState rel ()
--     vaultLookup :: () => rel -> Key -> VaultState rel (Maybe Val)
--     vaultDeleteAll :: Key -> VaultState rel ()
--     vaultDeleteVal :: rel -> Key -> VaultState rel ()
--     vaultFind :: Maybe rel -> Maybe Key -> Maybe Val -> VaultState rel [Row rel]
--     -- vaultAllDist :: (Eq rel, Ord rel) => Key -> VaultState rel [(rel, B4val)]
--     -- ^ get all distinctions with value, order is determined by enum type of rel
--     vaultCopyObj :: (Eq rel) => Key -> Key -> VaultState rel ()
--     vaultNew ::  Vault rel
--     vaultNewObjID :: VaultState rel Key
--     vaultKeyValid :: Key -> VaultState rel Bool
-- --    -- ^ check if key is in range first to last
--

    --     vaultInsert d k a  = modify (\v -> v{vstore= ntInsert k d a (vstore v)})
--     vaultLookup d k  =  get >>=
--                 return . listToMaybe . ntFindKR_V k d . vstore
-- --            -- does not guard against multiple results
-- ----                        _ -> errorT ["taxValGet", "more than one result", showT r]
--     vaultDeleteAll k   = modify (\v -> v{vstore=ntDeleteAll k (vstore v)})
--     vaultDeleteVal d k = modify (\v -> v{vstore=ntDeleteVal k d (vstore v)})
--     vaultFind md mk mv = get >>= return . ntFind mk md mv . vstore
-- --
-- --     vaultAllDist o = get >>=
-- --                     return . sort .  map (\abc -> (rr abc,  unVal4 . rv $ abc))
-- -- --                                -- take row appart rk rr rv
-- --                         . filter (isVal4 . rv)
-- --                         . ntFind (Just o) Nothing Nothing  . vstore
--     -- vaultCopyObj kold knew =
--     --         modify (\v -> v{vstore = storeCopyKeyData kold knew (vstore v)})

--     vaultNew = Vault { vstore = newNaiveStore
--                          , vfirst = 1
--                          , vlast = 0
--                          }
-- --
--     -- vaultNewObjID = do
--     --         v <- get
--     --         let n = vlast v + 1
--     --             v1 = v{vlast=n}
--     --         put v1
--     --         return $ makeObjID n
--     -- vaultKeyValid (Key i) = get >>= return . \v -> i >= vfirst v && i <= vlast v
-- --
