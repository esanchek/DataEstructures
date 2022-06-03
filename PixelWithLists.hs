----------------------------------------------------------------
-- Implementation of Pixels using lists
-- Author: Eduardo Sanchez Karhunen <fesanchez at us.es>
--         Assistant Professor University of Seville (Spain)
-- last modified: 01.06.2022
----------------------------------------------------------------

module PixelWithLists
  (Pix,
   fromList,      -- [Int] -> Pix  
   transpose,     -- Pix -> Pix
   row,           -- Pix -> Int
   col            -- Pix -> Int
  ) where
   
----------------------------------------------------------------
-- Def: A pixel is a 2-list from natural numbers (Nat)
-- Can be considered a 2-dim version of Nat numbers
-- e.g. a = [3,5]
----------------------------------------------------------------

type Pix = [Int]

fromList :: [Int] -> Pix
fromList (n:m:[]) = [n,m]
fromList _ = error "Pixel must have 2 elements"

---------------------------------------------------------------------           
-- Def: The transpose of the pixel a = [m,n], is the pixel aT = [n,m]
-- e.g. transpose [2,3] = [3,2]
---------------------------------------------------------------------

transpose :: Pix -> Pix
transpose (n:m:[]) = [m,n]
transpose _ = error "Pixel must have 2 elements"

---------------------------------------------------------------------           
-- Def: If a is a pixel a = [m,n]
--    a) row of pixel a is the first entry     r(a) = m
--    b) column of a pixel is the second entry c(a) = n
-- e.g. row [2,3] = 2
--      col [2,3] = 3
---------------------------------------------------------------------

row :: Pix -> Int
row (m:n:[]) = m

col :: Pix -> Int
col (m:n:[]) = n
