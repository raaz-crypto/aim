{-|

Internal module that captures the abstract syntax of the assembly
language. The assembly language here is mostly type less and hence
users of @aim@ should avoid using it directly.

-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}

module Aim.Assembler.Internal.Language
       ( Declaration(..), Array(..), Function(..), Scope(..)
       , Statement(..), Arg(..), VarDec(..)
       -- * Helpers to create immediate arguments
       , word8, word16, word32, word64, word128, word256, char8
       -- * Constants
       , Constant(..)
       -- * Stuff with comments
       , Commented(..), (<#>), (<!>)
       -- * Some Monoids
       , Declarations, Statements, CommentMonoid

       ) where

import Data.String
import Data.Text             ( Text, unpack                  )
import Data.Word             ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr           ( Ptr                           )

import Aim.Machine

-- | A program for a given machine.
type Declarations machine  = CommentMonoid (Declaration machine)

-- | A statement block for a given machine.
type Statements   machine = CommentMonoid (Statement   machine)

-- | A declaration is either an array or a function definition.
data Declaration machine = Verbatim Text -- ^ copy verbatim.
                         | DArray (Array machine)
                                      -- ^ An integral array
                                      -- declaration
                         | DFun (Function machine)
                                      -- ^ A function definition
                         deriving Show

-- | An array.
data Array machine = Array { arrayName      :: Text
                           , arrayValueSize :: Size
                           , arrayContents  :: [Integer]
                           } deriving Show
-- | A function.
data Function machine =
  Function { functionName       :: Text
           , functionScope      :: Scope machine
           , functionBody       :: Statements machine
           } deriving Show

-- | Argument, local variables and register allocated for use in the
-- function. This, in particular, determines the contents of the stack
-- of a functional call.
data Scope machine = Scope { scopeParams        :: [VarDec]
                           , scopeLocalVars     :: [VarDec]
                           , scopeRegisterAlloc :: [RegAlloc machine]
                           } deriving Show

-- | An statement can take 0,1,2 or 3 arguments. The text field is the
-- neumonic of the instruction.
data Statement machine = S0 Text
                       | S1 Text (Arg machine)
                       | S2 Text (Arg machine) (Arg machine)
                       | S3 Text (Arg machine) (Arg machine) (Arg machine)
                       deriving Show

-- | An argument of an assembly statement.
data Arg machine where
  -- An immediate value
  Immediate :: Constant -> Arg machine

  -- A parameter. The integer denote the offset in the parmeter
  -- stack. A function having three arguments @foo@, @bar@ and @biz@
  -- will have @foo@ as parameter 0, @bar@ as parameter 1 and @biz@ as
  -- parameter 2.
  Param     :: Int -> Arg machine

  -- A local variable (available on the stack). A convention similar
  -- to parameter is used here.
  Local     :: Int -> Arg machine

  -- A machine register.
  Reg       :: (Register reg, MachineConstraint machine reg)
            => reg -> Arg machine

  -- An indirect address. The base address is stored in a register
  -- that can hold a pointer.
  Indirect  :: ( Register reg
               , MachineConstraint machine reg
               , Type reg ~ Ptr a
               , MachineType a
               )
               => reg
               -> Int
               -> Arg machine

instance Show (Arg machine) where
  show (Immediate c)  = show c
  show (Param     i)  = "$(" ++ show i ++ ")"
  show (Local     i)  = "@(" ++ show i ++ ")"
  show (Reg       r)  = unpack (registerName r)
  show (Indirect r i) = unpack (registerName r)
                        ++ "[ " ++ show i ++ " ]"

-- | A variable declaration.
data Var ty = Var Text deriving Show

data VarDec where
  VarDec :: MachineType ty => Var ty -> VarDec

data RegAlloc machine where
  RegAlloc :: (Register reg, MachineConstraint machine reg)
           => reg -> RegAlloc machine

instance Show VarDec where
  show (VarDec v) = show v

instance Show (RegAlloc machine) where
  show (RegAlloc reg) = unpack $ registerName reg

------------------- Some helper functions --------------------

-- | An 8-bit unsigned integer.
word8  :: Word8  -> (Arg arch)
word8  = Immediate . I Size8 . toInteger

-- | A 16-bit unsigned integer.
word16 :: Word16 -> (Arg arch)
word16 = Immediate . I Size16 . toInteger

-- | A 32-bit unsigned integer.
word32 :: Word32 -> (Arg arch)
word32 = Immediate . I Size32 . toInteger

-- | A 64-bit unsiged integer.
word64 :: Word64 -> (Arg arch)
word64 = Immediate . I Size64 . toInteger

-- | A 128-bit unsigned integer.
word128 :: Integer -> (Arg arch)
word128 = Immediate . I Size128

-- | A 256-bit unsiged integer.
word256 :: Integer -> (Arg arch)
word256 = Immediate . I Size256

-- | Encode a character into its ascii equivalent.
char8 :: Char -> (Arg arch)
char8 = word8 . toEnum . fromEnum

--------------------- Constants ----------------------------------------


-- | A constant.
data Constant = I Size Integer  -- ^ A signed integer
              | F Double        -- ^ A floting point constant.
              deriving Show

------------------ Commenting ------------------------------------------

-- | An element that can be tagged with a comment.
data Commented a = Comment (Maybe a) Text deriving Show

-- | The comments monoid
type CommentMonoid a = [Commented a]

instance Functor Commented where
  fmap f (Comment ma txt) = Comment (fmap f ma) txt

instance IsString (Commented a) where
  fromString = Comment Nothing . fromString

-- | Comment an object.
(<#>) :: a -> Text -> Commented a
(<#>) x = Comment (Just x)

-- | Comments first and then the object.
(<!>) :: Text -> a -> Commented a
(<!>) = flip (<#>)
