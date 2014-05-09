{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}
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
       -- * Basic machine types
       -- $machinetypes$
       , WordSize, Size(..)
       , MachineType(..)
       , Supports
       , Register(..)
       , Operand (..)
       ) where

import GHC.Exts         ( Constraint                    )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Text        ( Text                          )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr      ( Ptr                           )

-- | Class that captures an architecture.
class Arch arch

-- | Class that captures a machine.
class Arch (ArchOf machine) => Machine machine where
  type ArchOf machine  :: *

-- $machinetypes$
--
-- In assembly language, we have 8-bit, 16-bit, 32-bit, 64-bit and
-- sometimes 128-bit and 256-bit signed and unsigned integral
-- values. Depending on the word size of the processor, some of these
-- types can be loaded on general purposes registers of the
-- machine. However, we want the code written in aim to be as type
-- safe as possible. We allow the use opaque types that can be encoded
-- as one of the above types. For example, we would want to treat
-- `Char8` differently than `Word8` even though they are encoded
-- similary on the machine. We capture this type safety by the classes
-- `MachineType` for types and `WordSize` for machines. Any type that
-- can potentialy be stored in the general purpose registers of some
-- machine has to be an instance of the class `MachineType`. The
-- associated type `TypeSize` captures the size in bits of the type in
-- the type level. This is done through the data kind obtained by
-- promoting the type `Size`. Each machine should define the word sizes
-- it can store in its registers by declaring instances of the class
-- `WordSize`. Finally, the constraint, @`Supports` mach ty@ captures
-- whether the machine @mach@ supports the type @ty@


-- | Different sizes that are available on the processor.
data Size = Size8
          | Size16
          | Size32
          | Size64
          | Size128
          | Size256
          | SizePtr  -- ^ Size of the pointer
          deriving Show


-- | Declaring a type to be an instance of `MachineType` means that it
-- can potentially be stored and processed in some machines registers.
-- The Associated type `TypeSize` captures the Size required to
-- capture the given type @ty@
class MachineType ty where
  type TypeSize ty :: Size

instance MachineType  Word8 where type TypeSize Word8  = Size8
instance MachineType   Int8 where type TypeSize  Int8  = Size8

instance MachineType Word16 where type TypeSize Word16 = Size16
instance MachineType  Int16 where type TypeSize  Int16 = Size16

instance MachineType Word32 where type TypeSize Word32 = Size32
instance MachineType  Int32 where type TypeSize  Int32 = Size32

instance MachineType Word64 where type TypeSize Word64 = Size64
instance MachineType  Int64 where type TypeSize  Int64 = Size64

instance MachineType a => MachineType (Ptr a) where
  type TypeSize (Ptr a) = SizePtr

-- | The instance @`WordSize` mach sz@ means that the machine @mach@
-- can process integral values of size @sz@.
class WordSize machine (sz :: Size)


-- | Supports is the constraint type that captures when a machine
-- supports a given machine type.
type Supports machine ty = ( MachineType ty
                           , WordSize machine (TypeSize ty)
                           )

-- | An operand of a machine.
class ( Arch (SupportedOn operand)
      , MachineType (Type operand)
      ) => Operand operand where

  -- | The architecture on which this operand is supported.
  type SupportedOn operand :: *

  -- | The type of the operand.
  type Type operand :: *

  -- | The constraint on the machine for this operand to exist. The
  -- default constraint is that the machine and the register have the
  -- same underlying arch and the type of the operand is supported by
  -- the machine.
  type MachineConstraint machine operand :: Constraint

  type MachineConstraint machine operand
          = ( Supports machine (Type operand)      --  machine should
                                                   --  support the
                                                   --  type
            , SupportedOn operand ~ ArchOf machine -- the should share
                                                   -- the underlying
                                                   -- architecture
            )

-- | Constraint that the given operand is a register.
class Operand operand => Register operand where
  -- | Name of the register
  registerName :: operand -> Text
