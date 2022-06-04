----------------------------------------------------------------
-- Author: Eduardo Sanchez Karhunen
-- Assistant Professor University of Seville (Spain) 
----------------------------------------------------------------

import PixelWithLists as Pixel
import MaxelWithMSets as Maxel
import Test.QuickCheck

----------------------------------------------------------------
-- PIXEL PROPERTIES
----------------------------------------------------------------
----------------------------------------------------------------
-- Def: A pixel of the form [n,n] is diagonal
-- e.g. isDiagonalPixel (Pixel.fromList [2,2]) == True
----------------------------------------------------------------

isDiagonalPixel :: Pix -> Bool
isDiagonalPixel p = row p == col p

----------------------------------------------------------------
-- A pixel a is diagonal <=> a = a.T
-- e.g. prop_diagonal_pixel (Pixel.fromList [1,1]) == True
--      prop_diagonal_pixel (Pixel.fromList [1,2]) == False
----------------------------------------------------------------

prop_diagonal_pixel :: Pix -> Bool
prop_diagonal_pixel p = Pixel.transpose p == p

----------------------------------------------------------------
-- A pixel a, (a.T).T = a
-- e.g. prop_idempotens (fromList [1,2]) == True
----------------------------------------------------------------

prop_idempotens :: Pix -> Bool
prop_idempotens p = ((Pixel.transpose).(Pixel.transpose)) p == p

---------------------------------------------------------------------               
-- Def: Pixels a=[k,l] and b=[m,n] are:
--    a) row-collinear <=> k = m
--    b) column-collinear <=> l = n
--    c) collinear <=> they are either row-collinear or col-collinear.
-- e.g. collinear (Pixel.fromList [1,3]) (Pixel.fromList [1,5])
--      collinear (Pixel.fromList [1,3]) (Pixel.fromList [4,3])
---------------------------------------------------------------------

collinear :: Pix -> Pix -> Bool 
collinear p1 p2 = (row p1 == row p2) || (col p1 == col p2)

----------------------------------------------------------------
-- Def: A maxel m is diagonal <=> all its pixels are diagonal.
-- e.g. isDiagonalMaxel (Maxel.fromLists [[2,2],[1,1]]) == True
--      isDiagonalMaxel (Maxel.fromLists [[2,3],[1,1]]) == False
----------------------------------------------------------------

isDiagonalMaxel :: Max -> Bool
isDiagonalMaxel m = and [ isDiagonalPixel p | p <- toList m]

----------------------------------------------------------------
-- Def: A maxel m is symmetric <=> m.T =m.
-- e.g. 
----------------------------------------------------------------

isSymmetric :: Max -> Bool
isSymmetric m = Maxel.transpose m == m

----------------------------------------------------------------
-- Def: If extent m = [r,c] then the extent of the transpose is
-- the transpose of the extent.
----------------------------------------------------------------

prop_extent_transpose m = a == b
  where a = extent m
        b = Pixel.transpose (extent (Maxel.transpose m))
