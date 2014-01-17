{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
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
       (
       -- * Architecture and Machine.
         Arch, Machine(..)
       , Supports
       , Instruction, Instructions, assembleInstructions
       , comments, (<#>)
       -- * Basic machine types
       -- $basicmachinetypes$
       , Type8Bits, Type16Bits, Type32Bits, Type64Bits, Type128Bits
       , Register
       , Operand (..)
       ) where

import GHC.Exts         ( Constraint                    )
import Data.Text        ( Text, unlines, lines, length
                        , replicate, null
                        )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Monoid
import Data.String      ( IsString(..)                  )
import Prelude hiding   ( unlines, lines, length
                        , replicate, null
                        )



class Arch arch

-- | Class that captures a machine.
class Arch (ArchOf machine) => Machine machine where
  type ArchOf machine :: *

-- | Whether the machine supports values of this type.
class Machine machine => Supports machine typ

data Instruction machine = Instruction Text

instance IsString (Instruction machine) where
  fromString = Instruction . fromString

-- | A sequence of instructions.
type Instructions machine = [Instruction machine]

-- | Assemble a sequence of instructions into the corresponding
-- assembly language program.
assembleInstructions :: Instructions machine -> Text
assembleInstructions = unlines . map text
  where text (Instruction txt) = txt

-- | Generate a comment block
comments :: Text -> Instructions machine
comments txt = [ Instruction $ "# " <> t | t <- lines txt ]

-- | Comment on an instruction
(<#>) :: Instruction machine -- ^ The instruction to comment on
      -> Text                -- ^ The comment
      -> Instruction machine
(<#>) inst@(Instruction txt) com
  | null com  =  inst
  | otherwise = Instruction $ unlines $ first : rest
  where pad    = replicate (length txt + 3) " "
        (c:cs) = lines com
        first  = txt <> replicate 3 " " <> "# " <> c
        rest   = [pad <> "# " <> x | x <- cs]

-- $basicmachinetypes$
--
-- In assembly language, the basic units of information 8-bit, 16-bit,
-- 32-bit, 64-bit and sometimes 128-bit signed and unsigned
-- integers. However, to improve type safety, we should be able to use
-- opaque types that can be encoded as one of the above types. For
-- example, we would want to treat Char8 differently than Word8 even
-- though they are encoded similary on the machine. We support using
-- any of these opaque types in assembly instructions. The classes
-- `Type8Bits`, `Type16Bits`, `Type32Bits`, `Type64Bits` and
-- `Type128Bits` are meant to capture these opaque types.

-- | Types that are essentially 8-bit quantities.
class Type8Bits typ

instance Type8Bits Word8; instance Type8Bits Int8

-- | Types that are essentially 16-bit quantities.
class Type16Bits typ
instance Type16Bits Word16; instance Type16Bits Int16

-- | Types that are essentially 32-bit quantities
class Type32Bits typ
instance Type32Bits Word32; instance Type32Bits Int32

-- | Types that are essentially 64-bit quantities
class Type64Bits typ
instance Type64Bits Word64; instance Type64Bits Int64

-- | Types that are essentially 128-bit quantities
class Type128Bits typ

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
                                           , SupportedOn operand ~ ArchOf machine
                                           )

-- | Constraint that the given operand is a register.
class Operand operand => Register operand
