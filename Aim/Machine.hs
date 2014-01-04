{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-|

One of the important consideration in the design of Aim is that errors
that occur by the mixing of incompatible instructions has to be
reported at compile time rather than at runtime. This requires a
systematic way to constraint assembler instructions depending on stuff
like architecture, specific features that might be supported by the
architecture (e.g.  SSE). This module facilitates capturing such
constraints at the type level.

-}

module Aim.Machine
       ( Arch, Machine, Supports
       , Register
       , Bits32, Bits64
       , Instruction(..), Operand (..)
       ) where

import GHC.Exts         ( Constraint                    )
import Data.Text        ( Text                          )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )


class Arch arch

-- | Class that captures a machine.
class Arch (ArchOf machine) => Machine machine where
  type ArchOf machine :: *

-- | Whether the machine supports values of this type.
class Machine machine => Supports machine typ

data Instruction machine = Instruction Text

-- | Constraints that captures a 32-bit machine. It supports both
-- signed and unsigned ints of size upto 32.
type Bits32 machine = ( Supports machine Word8 , Supports machine Int8
                      , Supports machine Word16, Supports machine Int16
                      , Supports machine Word32, Supports machine Int32
                      )

-- | Constraints that captures a 64-bit machine. It supports both
-- signed and unsigned ints of size upto 64.
type Bits64 machine  = ( Supports machine Word64, Supports machine Int64
                       , Bits32   machine
                       )


-- | This constraint asserts that the register and the machine has the
-- same underlying architecture.
type SameArchitectures machine operand = ( Machine machine
                                         , Operand operand
                                         , SupportedOn operand ~ ArchOf machine
                                         )

-- | Class that captures all possible operands. Operands have a type
-- and they are usually suppored on a specific architecture. They also
-- put additional constraints on the underlying machine.
class Arch (SupportedOn operand) => Operand operand where

  -- | The architecture on which this operand is supported.
  type SupportedOn operand :: *

  -- | The type of the operand.
  type Type operand :: *

  -- | The constraint on the machine for this operand to exist. The
  -- default constraint is that the machine and the register have the
  -- same underlying arch.
  type MachineConstraint machine operand :: Constraint

  type MachineConstraint machine operand = ( Supports machine (Type operand)
                                           , SameArchitectures machine operand
                                           )

-- | Constraint that the given operand is a register.
class Operand operand => Register operand
