----------------------------------------------------------------
-- Implementation of Pixels using lists
-- Author: Eduardo Sanchez Karhunen <fesanchez at us.es>
--         Assistant Professor University of Seville (Spain)
-- last modified: 01.06.2022
----------------------------------------------------------------   
---------------------------------------------------------------
-- Def: A maxel is a multiset of pixels
-- e.g. m = [[1,1],[1,1],[1,3],[1,4],[2,2],[2,2],[2,4]]
---------------------------------------------------------------

module MaxelWithMSets
    (Max,
     MaxelWithMSets.fromList,  -- [Pix] -> Max
     fromLists,                -- [[Int]] -> Max
     toList,                   -- Max -> [Pix]
     MaxelWithMSets.transpose, -- Max -> Max
     size,                     -- Max -> Int
     extent                    -- Max -> Pix
    ) where 

import PixelWithLists as Pixel
import qualified Data.MultiSet as Mset

type Max = Mset.MultiSet Pix

fromList :: [Pix] -> Max
fromList ps = Mset.fromList ps

fromLists :: [[Int]] -> Max
fromLists [] = Mset.empty
fromLists (x:xs) = Mset.insert (Pixel.fromList x) (fromLists xs)  

toList :: Max -> [Pix]
toList m = Mset.toList m

---------------------------------------------------------------
-- Def: The size of a maxel is the number of its elements,
-- repetitions included, denoted |m|. E.g.:
--    size m1 == 7
---------------------------------------------------------------

size :: Max -> Int
size m = Mset.size m 

---------------------------------------------------------------
-- Def: The extent of a maxel m is the pixel e(m)=[r,c] where
-- r and c are the largest rows and columns of pixels in m,
-- respectively. E.g.:
--    extent m1 == [2,4]
---------------------------------------------------------------

extent :: Max -> Pix
extent m = [r,c]
  where r = maximum [ row pixel | pixel <- Mset.elems m ]
        c = maximum [ col pixel | pixel <- Mset.elems m ]

---------------------------------------------------------------
-- Def: The transpose of a maxel m is the maxel m.T of transposes
-- of its pixels. e.g.:
--    Maxel.transpose (Maxel.fromLists [[1,1],[2,3],[2,3],[5,1]])
--      == Maxel.fromLists [[1,1],[3,2],[3,2],[1,5]]
---------------------------------------------------------------

transpose :: Max -> Max
transpose m = fromLists [Pixel.transpose pixel | pixel <- Mset.toList m]
