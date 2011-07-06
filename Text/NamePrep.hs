module Text.NamePrep where

import Text.StringPrep

namePrepProfile :: Bool -> StringPrepProfile
namePrepProfile allowUnassigned =
	Profile {
		maps = [b1,b2],
		shouldNormalize = True,
		prohibited = (if allowUnassigned then [] else [a1]) ++ [c12,c22,c3,c4,c5,c6,c7,c8,c9],
		shouldCheckBidi = True
	}
