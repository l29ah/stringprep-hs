{-# LANGUAGE PatternGuards #-}
module Text.CharRanges where

import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set

data Range = Single {-# UNPACK #-} !Char
           | Range  {-# UNPACK #-} !Char {-# UNPACK #-} !Char

-- | A rather hacked-up instance.
--   This is to support fast lookups using 'Data.Set' (see 'toSet').
instance Eq Range where
	(Single x) == (Single y) = x == y
	(Single a) == (Range x y) = x <= a && a <= y
	(Range x y) == (Single a) = x <= a && a <= y
	(Range lx ux) == (Range ly uy) = (lx <= uy && ux >= ly) || (ly <= ux && uy >= lx)

instance Ord Range where
	(Single x) <= (Single y) = x <= y
	(Single x) <= (Range y _) = x <= y
	(Range _ x) <= (Single y) = x <= y
	(Range _ x) <= (Range y _) = x <= y

-- | Allows quick lookups using ranges.
toSet :: [Range] -> Set Range
toSet = Set.fromDistinctAscList . prepareRanges
  where prepareRanges :: [Range] -> [Range]
        prepareRanges =  go . sort
        go (r1:r2:rs) | Just r' <- maybeMergeRanges r1 r2 = go (r':rs)
                      | otherwise = r1 : go (r2:rs)
        go rs = rs
        maybeMergeRanges :: Range -> Range -> Maybe Range
        maybeMergeRanges x y = if x == y
                               then Just $ minMax x y
                               else Nothing

minMax :: Range -> Range -> Range
minMax (Range lx ux) (Range ly uy) = Range (min lx ly) (max ux uy)
minMax (Single _) y = y
minMax x@(Range _ _) (Single _) = x
{-# INLINE minMax #-}

range :: Char -> Char -> Range
range x y = if x < y then Range x y
            else error "range: x not smaller than y"
{-# INLINE range #-}

single :: Char -> Range
single = Single
{-# INLINE single #-}