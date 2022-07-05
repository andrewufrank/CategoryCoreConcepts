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

module Lib.CCdata
     where -- normal prelude
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
import qualified Data.Map.Strict as Map
import Data.List (nub)
import GHC.Base

import Vault.Value


pageCCdata :: IO ()
pageCCdata= do
    putIOwords ["\npageCCdata"]
    putIOwords ["field field1", showT  field1]
    putIOwords ["at2", showT at2]
    putIOwords ["o1, o2", showT o1, showT o2]
    putIOwords ["values f1 t1", showT f1, showT t1]
    -- putIOwords ["coords", showT coords]
    -- putIOwords ["p2c", showT . map (p2c pointData) $ ps]
    -- putIOwords ["c2p", showT . map (c2p pointData) $ cs]
    -- putIOwords ["injective pointData", showT . injectiveTest $ pointData]

------------------------------------------------ base data types

-------- Point

data Point1d a = Point1d {x1 :: a}
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
type Point  = Point1d Float 
mkpt :: Float -> Point 
mkpt = Point1d  -- x1 . mkpt = id 

instance Functor Point1d  where
    fmap f = Point1d . f . x1
instance Applicative Point1d where  
    liftA2 f (Point1d x) (Point1d y) = Point1d (f x y)
    pure = Point1d
        
instance (Num f) => Num (Point1d f) where
    (+)  = liftA2 (+)
    (*) = liftA2 (*)
    negate         = fmap negate
    fromInteger i  = Point1d . fromInteger $ i
    abs    = fmap abs
    signum = fmap signum


--------------------- field

data Field = Field {base :: Float, tang :: Float}
        deriving (Eq, Ord, Read, Show, Generic, Zeros)
-- ^ Field with trival interpolation f x = base + x * tang

fieldval :: Field -> Point1d Float -> ValueSum
fieldval (Field b t) p = mkfloat $ b + x1 p * t 

field1 = Field 0 0.1
at2 = fieldval field1 (mkpt 2)

-- ------------------ objects
-- ^ Obj with a single value; a product type ID x Value
-- Value is a Sum type (ValueF + ValueT + ValueO) 
class Objects o v where
    mkobj :: Int -> v -> o
    getval :: o -> v
class Objects0 o where 
    getid :: o -> Int 
instance Objects Obj Float where 
    mkobj i v =  Obj (ID i) (mkfloat v)
instance Objects Obj Text where 
    mkobj i v =  Obj (ID i) (mktext v)
data Obj = Obj {oid::IDint, oval :: ValueSum}
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

o1, o2 :: Obj
o1 = mkobj 1 (3.2 :: Float)  
o2 = mkobj 2 ("object 2" :: Text) -- how to assure unique id?


-------------------------old

-- data WorldPoint = WP {space::Point1dFloat, val:: Float}
--     deriving (Eq, Ord, Read, Show)
-- mkwp x11 v11 = WP (mkp x11) v11
-- wp1 = WP p1 t0 th0

-- p1 = Point1d 1.0


-- type Field = Map.Map WorldPoint Value

-- f1 = Map.fromList [(wp1, mkvi 0),  ( mkwp 1.2 2, mkvi 1)]


-- type Objects = Map.Map ObjProperty Value


-- data Field = Field Float Float
-- field1 = Field 1.0 0.1 

-- field :: Field -> Point -> Float
-- field (Field a b) p = a + x p * b 

-- -- necessary because set has context Ord 
-- type Ranking = ConstrainedCategory (->) Ord
-- ordd :: (Ord a, Ord b) => (a -> b) -> Ranking a b
-- ordd = constrained
-- inOrd :: Ranking a b -> Ranking a b
-- inOrd = id
-- instance Functor Set Ranking Ranking where
--   fmap = constrainedFmap Set.map

-- pointData :: Map.Map Text Float
-- pointData = Map.fromList $ zip ["a1", "a2"] [1.0, 2.0]
-- -- is automatically surjective (if no other def of domain)

-- injectiveTest :: (Eq a, Eq b) => Map.Map a b -> Bool
-- injectiveTest mp = length ls == (length . nub $ ls)
--     where ls = map snd. Map.toList $ mp


-- points :: Set Text
-- points = Set.fromList $ Map.keys pointData

-- ps :: [Text]
-- ps = Set.toList points

-- coords :: Set Float
-- coords = Set.fromList $ Map.elems pointData

-- cs :: [Float]
-- cs = Set.toList coords

-- p2c :: Map.Map Text Float -> Text -> Float
-- p2c pdb p = pdb Map.! p

-- c2p :: Map.Map Text Float -> Float -> Text
-- c2p pdb c = (Map.fromList . map swap . Map.toList $ pdb) Map.! c















--     -- putIOwords ["y1", showT y1]
-- -- x :: Set Integer
-- -- x = Set.fromList [1 .. 9]
-- --     -- deriving  (Show, Eq, Bounded, Enum, Ord)
-- --     --  diagram page 137
-- -- y :: Set Integer
-- -- y = Set.fromList [11..19]

-- -- y1 :: Set Integer
-- -- y1 = fmap (ordd(+10)) $ x 


-- -- f137 :: SetX -> SetX
-- -- f137 = fromPfeile [(X1,X2), (X2,X3), (X3,X4), (X4, X5), (X5,X3), (X6,X7), (X7,X5), (X8,X9),(X9,X9)]


-- -- data SetY a = SetY a 
-- --     deriving  (Show, Eq, Bounded, Ord)  -- not Enum

-- -- instance Hask.Functor SetY  where
-- --     fmap ff (SetY a) = SetY (ff a)
-- -- --     -- fmap . f137 = f137' . fmap

-- -- f137' :: SetY SetX -> SetY SetX
-- -- f137' = fmap f137 

-- -- fp137' ::  [SetY SetX]
-- -- fp137' = fixedPoints f137'

-- -- page135hask :: IO ()
-- -- page135hask= do
-- --     putIOwords ["\npage 135hask"]
-- --     putStr "Functor:  " 
-- --     print $ fmap (ordd (^2)) $ Set.fromList [-3 .. 4]
-- --     putIOwords ["y", showT y]
-- --     putIOwords ["y1", showT y1]
--      -- putIOwords ["toPfeile f ", showT (toPfeile f137)]
--     -- putIOwords ["injective f", showT (injective f137)]
--     -- putIOwords ["surjective f", showT (surjective f137)]
--     -- putIOwords ["countSections f", showT (countSections f137)]
--     -- putIOwords ["naming f ", showT (naming f137)]
--     -- putIOwords ["stacking f ", showT (stacking f137)]
--     -- putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
--     -- putIOwords ["sorting f ", showT (sorting f137)]
--     -- -- -- the map f137 A -> A has a codomain of the fixed points and
--     -- -- -- the sorting is then done with this codomain
--     -- putIOwords ["fixedPoints f' ", showT (fp137')]
--     -- return ()


