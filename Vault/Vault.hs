-----------------------------------------------------------------------------
--
-- Module      :  VaultState storage
-- uses the naive triplestore
-- and wraps in state monad
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

module Vault.Vault (
    Vaults (..), Vault (..), VaultState
    -- , ObjID, makeObjID
--    , module Vault.NaiveTriplestore
--    ,  Rel4Test  (..)
    -- , B4val (..), proxyB4val, proxyFloat
    , Val
    , Vals (..)
    -- ,  unVal4, isVal4
    , Row (..)
--    , newNaiveStore -- for testing
    )    where

import Control.Monad.State
-- import Data.List (sort)
import UniformBase
import Vault.NaiveTripleStore

data Vault rel = Vault { vstore :: Store rel
                   , vfirst :: Int
                   , vlast :: Int
                   }  deriving (Show, Read, Eq)

type ObjID = Key
makeID :: Text -> Key
makeID i = Key i

type VaultState rel = State (Vault rel)

class Vaults rel where
    vaultInsert :: () => rel -> Key -> Val -> VaultState rel ()
    vaultLookup :: () => rel -> Key -> VaultState rel (Maybe Val)
    vaultDeleteAll :: Key -> VaultState rel ()
    vaultDeleteVal :: rel -> Key -> VaultState rel ()
    vaultFind :: Maybe rel -> Maybe Key -> Maybe Val -> VaultState rel [Row rel]
    -- vaultAllDist :: (Eq rel, Ord rel) => Key -> VaultState rel [(rel, B4val)]
    -- ^ get all distinctions with value, order is determined by enum type of rel
    vaultCopyObj :: (Eq rel) => Key -> Key -> VaultState rel ()
    vaultNew ::  Vault rel
    vaultNewObjID :: VaultState rel Key
    vaultKeyValid :: Key -> VaultState rel Bool
--    -- ^ check if key is in range first to last
--
--
instance (NaiveTriples rel) => Vaults rel where
    vaultInsert d k a  = modify (\v -> v{vstore= ntInsert k d a (vstore v)})
    vaultLookup d k  =  get >>=
                return . listToMaybe . ntFindKR_V k d . vstore
--            -- does not guard against multiple results
----                        _ -> errorT ["taxValGet", "more than one result", showT r]
    vaultDeleteAll k   = modify (\v -> v{vstore=ntDeleteAll k (vstore v)})
    vaultDeleteVal d k = modify (\v -> v{vstore=ntDeleteVal k d (vstore v)})
    vaultFind md mk mv = get >>= return . ntFind mk md mv . vstore
--
--     vaultAllDist o = get >>=
--                     return . sort .  map (\abc -> (rr abc,  unVal4 . rv $ abc))
-- --                                -- take row appart rk rr rv
--                         . filter (isVal4 . rv)
--                         . ntFind (Just o) Nothing Nothing  . vstore
    -- vaultCopyObj kold knew =
    --         modify (\v -> v{vstore = storeCopyKeyData kold knew (vstore v)})

    vaultNew = Vault { vstore = newNaiveStore
                         , vfirst = 1
                         , vlast = 0
                         }
--
    -- vaultNewObjID = do
    --         v <- get
    --         let n = vlast v + 1
    --             v1 = v{vlast=n}
    --         put v1
    --         return $ makeObjID n
    -- vaultKeyValid (Key i) = get >>= return . \v -> i >= vfirst v && i <= vlast v
--
