------------------------------------------------------
-- Author: Eduardo Sanchez Karhunen
-- Assistant Professor University of Seville (Spain) 
------------------------------------------------------

import Test.QuickCheck
------------------------------------------------------
-- Def: A pixel is a 2-list from natural numbers (Nat)
-- Can be considered a 2-dim version of Nat numbers
-- e.g. a = [3,5]
------------------------------------------------------

type Pix = [Int]   -- study why not with tuple

-----------------------------------------------------
-- Def: A maxel is a multiset of pixels
-- e.g. m = [[3,5],[3,5],[2,2]]
-----------------------------------------------------

type Max = [Pix]   -- study why not use multiset

------------------------------------------------------
-- Def: A pixel of the form [n,n] is diagonal
------------------------------------------------------

isDiagonal :: Pix -> Bool
isDiagonal (x:y:[]) = x == y
isDiagonal _ = error "Pixel must have 2 elements"

------------------------------------------------------
-- a is diagonal si y solo si: a = a.T
------------------------------------------------------

prop_diagonal :: Pix -> Bool
prop_diagonal p = transpose p == p

------------------------------------------------------               
-- Def: The transpose of the a = [m,n], is the pixel aT = [n,m]
------------------------------------------------------

transpose :: Pix -> Pix
transpose (x:y:[]) = [y,x]
transpose _ = error "Pixel must have 2 elements"



