{-|

This module supports generation of C Code via aim.

-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE EmptyDataDecls         #-}

module Aim.Arch.C
       ( C
       , CRegister
       ) where

import Data.Text

import Aim.Machine

-- | The portable C architecture.
data C

-- | C is an instance of Arch.
instance Arch C

-- | There is only one possible machine for the C architecture.
instance Machine C where
  type ArchOf C = C

-- | C supports  8-bit integrals.
instance WordSize C Size8
-- | C supports 16-bit integrals.
instance WordSize C Size16
-- | C supports 32-bit integrals.
instance WordSize C Size32
-- | C supports 64-bit integrals.
instance WordSize C Size64
-- | C supports pointer types.
instance WordSize C SizePtr

-- | A C register.
data CRegister ty = CRegister Text

instance MachineType ty => Operand (CRegister ty) where

  type SupportedOn (CRegister ty) = C
  type Type (CRegister ty) = ty

instance MachineType ty => Register (CRegister ty) where
         registerName (CRegister rName) = rName
