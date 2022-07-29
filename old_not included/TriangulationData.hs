-----------------------------------------------------------------------------
--
-- Module      :  the data for triangulation tests
{-  

 only the data and the output
 -- this is for the attempt to use hgeometry
-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass     #-}

module ExampleData.TriangulationData

 
     where

-- change prelude for constrainded-categories
-- import Prelude ()
-- import Control.Category.Constrained.Prelude
 
-- import qualified Control.Category.Hask as Hask
-- import Control.Monad.Constrained  
-- -- end 

import UniformBase
    -- ( Generic, Zeros(zero), errorT, ErrIO, putIOwords, showT ) 
-- import Control.Monad.State

import Vault.Triple4cat
import Vault.TrisFiles

-- import GIS.Category
import Storable.Store
import Storable.Sobj  
import Storable.Makes
-- import GIS.Functions
import GIS.FunGeometry

--------------------data 



offset_two :: Int
offset_two = 0 :: Int

posTriple_two :: [[(ObjPoint, MorphPoint, ObjPoint)]]
posTriple_two = map (makeNode offset_two) pos_two
-- hqTriple_two :: [[(ObjPoint, MorphPoint, ObjPoint)]]
-- hqTriple_two = map (makeHQ offset_two)    $ edge_two
-- first effort - cannot decide on side of face
 

main3 :: IO ()
main3 =  do  -- with tests in other modules
    putIOwords ["\n main3\n"]
    putIOwords ["\n loading the data from triangulation two"] 
    putIOwords ["\n the node data", showT pos_two]
    putIOwords ["\n the edge data", showT edge_two]
    putIOwords ["\n the nodeTriple", triplePerLine posTriple_two]
    -- putIOwords ["\n the hqTriple", h2]
    return ()



catTwo0 :: CatStore ObjPoint MorphPoint
catTwo0 = catStoreEmpty
catTwo1 :: CatStore ObjPoint MorphPoint
catTwo1 = catStoreBatch (map wrapIns' . concat $ posTriple_two) catTwo0
-- catTwo2 :: CatStore ObjPoint MorphPoint
-- catTwo2 = catStoreBatch (map wrapIns' . concat $ hqTriple_two) catTwo1


{- 
loading the data from triangulation two
code tri_two, pos_two, edge_two 
then posTriple_two, edgeTriple_two
the node data [(0,(0.0,0.0,"11")),(1,(1.5,1.5,"12")),(2,(0.0,2.0,"13")),(3,(2.0,0.0,"14"))]
the edge data [(0,2),(0,1),(0,3),(1,3),(1,0),(1,2),(2,1),(2,0),(3,0),(3,1)]
the nodeTriple [[(NodeTag (Node "200"),XYtag XY,PointTag (Point2 0.0 0.0)),(NodeTag (Node "200"),Nametag,NameTag (Name "\"11\""))],[(NodeTag (Node "201"),XYtag XY,PointTag (Point2 1.5 1.5)),(NodeTag (Node "201"),Nametag,NameTag (Name "\"12\""))],[(NodeTag (Node "202"),XYtag XY,PointTag (Point2 0.0 2.0)),(NodeTag (Node "202"),Nametag,NameTag (Name "\"13\""))],[(NodeTag (Node "203"),XYtag XY,PointTag (Point2 2.0 0.0)),(NodeTag (Node "203"),Nametag,NameTag (Name "\"14\""))]]
the edgeTriple [[(HQTag (HQ 200 202),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 202),Stag S,NodeTag (Node "202"))],[(HQTag (HQ 200 201),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 201),Stag S,NodeTag (Node "201"))],[(HQTag (HQ 200 203),Stag S,NodeTag (Node "200")),(HQTag (HQ 200 203),Stag S,NodeTag (Node "203"))],[(HQTag (HQ 201 203),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 203),Stag S,NodeTag (Node "203"))],[(HQTag (HQ 201 200),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 201 202),Stag S,NodeTag (Node "201")),(HQTag (HQ 201 202),Stag S,NodeTag (Node "202"))],[(HQTag (HQ 202 201),Stag S,NodeTag (Node "202")),(HQTag (HQ 202 201),Stag S,NodeTag (Node "201"))],[(HQTag (HQ 202 200),Stag S,NodeTag (Node "202")),(HQTag (HQ 202 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 203 200),Stag S,NodeTag (Node "203")),(HQTag (HQ 203 200),Stag S,NodeTag (Node "200"))],[(HQTag (HQ 203 201),Stag S,NodeTag (Node "203")),(HQTag (HQ 203 201),Stag S,NodeTag (Node "201"))]]
-}