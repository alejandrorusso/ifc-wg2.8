module Labeled where

import Core
import LValue

type Labeled l a = LValue FI l a

unlabel :: Lattice l => Labeled l a -> IFC l a
unlabel lv = bless R return lv

label :: Lattice l => l -> a -> IFC l (Labeled l a)
label l x = newLValue l x