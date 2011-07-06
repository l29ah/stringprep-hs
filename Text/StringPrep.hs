{-# LANGUAGE CPP, OverloadedStrings #-}
module Text.StringPrep (
StringPrepProfile(..),
runStringPrep,
a1,
b1,b2,
c11,c12,c21,c22,c3,c4,c5,c6,c7,c8,c9
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU.Normalize (NormalizationMode(NFKC),normalize)
import Data.List.Stream
import Prelude hiding (any,concatMap,concat,foldr,map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ranges

data StringPrepProfile = Profile 
	{
		maps :: [Map],
		shouldNormalize :: Bool,
		prohibited :: [Prohibited],
		shouldCheckBidi :: Bool
	}

runStringPrep :: StringPrepProfile -> Text -> Maybe Text
runStringPrep (Profile maps norm prohibs bidi) s = result 
	where
		prohibited = toSet $ ranges $ concat prohibs
		mapped = foldr Text.concatMap s maps
		normed = if norm
			then normalize NFKC mapped
			else mapped
		bidid = if bidi
				then if checkBidi normed
					then Just normed
					else Nothing
				else Just normed
		result = case bidid of
			Nothing -> Nothing
			Just t -> if Text.any (\x -> Set.member (single x) prohibited) t
				then Nothing
				else Just t

checkBidi t = not containsRandL || not containsAL && firstRandL && lastRandL
	where
		containsRandL = Text.any (\x -> Set.member (single x) randl) t
		containsAL = Text.any (\x -> Set.member (single x) l) t
		firstRandL = Set.member (single (Text.head t)) randl
		lastRandL = Set.member (single (Text.last t)) randl

type Map = Char -> Text
type Prohibited = [Range Char]

b1 :: Map
b1 c =
	if c `Set.member` mapToNothings
		then Text.empty
		else Text.singleton c
			
mapToNothings = Set.fromAscList ['\x00AD', '\x034F', '\x1806', '\x180B', '\x180C','\x180D', '\x200B', '\x200C', '\x200D', '\x2060', '\xFE00', '\xFE01', '\xFE02','\xFE03', '\xFE04', '\xFE05', '\xFE06', '\xFE07', '\xFE08', '\xFE09', '\xFE0A', '\xFE0B', '\xFE0C', '\xFE0D', '\xFE0E', '\xFE0F', '\xFEFF']

#include "b2.hs"

b2 :: Map
b2 c = case Map.lookup c b2map of
	Nothing -> Text.singleton c
	Just t -> t

c11 = [single ' ']

c12 = map single ['\x00A0','\x1680','\x2000','\x2001','\x2002','\x2003','\x2004','\x2005','\x2006','\x2007','\x2008','\x2009','\x200A','\x200B','\x202F','\x205F','\x3000']

c21 = [range '\x0' '\x1f', single '\x7f']

c22 = [
	range '\x80' '\x9f',
	single '\x6dd', single '\x070F', single '\x180E',
	single '\x200C', single '\x200D', single '\x2028',
	single '\x2029', single '\x2060', single '\x2061',
	single '\x2062', single '\x2063',
	range '\x206a' '\x206f',
	single '\xfeff',
	range '\xfff9' '\xfffc',
	range '\x1d173' '\x1d17a']

c3 = [
	range '\xe000' '\xf8ff',
	range '\xf0000' '\xffffd',
	range '\x100000' '\x10fffd']

c4 = [
	range '\xFDD0' '\xFDEF',
	range '\xFFFE' '\xFFFF',
	range '\x1FFFE' '\x1FFFF',
	range '\x2FFFE' '\x2FFFF',
	range '\x3FFFE' '\x3FFFF',
	range '\x4FFFE' '\x4FFFF',
	range '\x5FFFE' '\x5FFFF',
	range '\x6FFFE' '\x6FFFF',
	range '\x7FFFE' '\x7FFFF',
	range '\x8FFFE' '\x8FFFF',
	range '\x9FFFE' '\x9FFFF',
	range '\xAFFFE' '\xAFFFF',
	range '\xBFFFE' '\xBFFFF',
	range '\xCFFFE' '\xCFFFF',
	range '\xDFFFE' '\xDFFFF',
	range '\xEFFFE' '\xEFFFF',
	range '\xFFFFE' '\xFFFFF',
	range '\x10FFFE' '\x10FFFF']


c5 = [range '\xd800' '\xdfff']

c6 = [range '\xfff9' '\xfffd']

c7 = [range '\x2ff0' '\x2ffb']

c8 = [
	single '\x340', single '\x341', single '\x200e', single '\x200f',
	range '\x202a' '\x202e', range '\x206a' '\x206f']
	
c9 = [single '\xe0001', range '\xe0020' '\xe007f']

randl = toSet $ ranges [
	single '\x05BE',
	single '\x05C0',
	single '\x05C3',
	range '\x05D0' '\x05EA',
	range '\x05F0' '\x05F4',
	single '\x061B',
	single '\x061F',
	range '\x0621' '\x063A',
	range '\x0640' '\x064A',
	range '\x066D' '\x066F',
	range '\x0671' '\x06D5',
	single '\x06DD',
	range '\x06E5' '\x06E6',
	range '\x06FA' '\x06FE',
	range '\x0700' '\x070D',
	single '\x0710',
	range '\x0712' '\x072C',
	range '\x0780' '\x07A5',
	single '\x07B1',
	single '\x200F',
	single '\xFB1D',
	range '\xFB1F' '\xFB28',
	range '\xFB2A' '\xFB36',
	range '\xFB38' '\xFB3C',
	single '\xFB3E',
	range '\xFB40' '\xFB41',
	range '\xFB43' '\xFB44',
	range '\xFB46' '\xFBB1',
	range '\xFBD3' '\xFD3D',
	range '\xFD50' '\xFD8F',
	range '\xFD92' '\xFDC7',
	range '\xFDF0' '\xFDFC',
	range '\xFE70' '\xFE74',
	range '\xFE76' '\xFEFC']

#include "l.hs"
#include "a1.hs"
