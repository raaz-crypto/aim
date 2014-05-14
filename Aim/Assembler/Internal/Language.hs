{-|

Internal module that captures the abstract syntax of the assembly
language. The assembly language here is mostly type less and hence
users of @aim@ should avoid using it directly.

-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}

module Aim.Assembler.Internal.Language
       ( Declaration(..), Array(..), Function(..), Scope(..)
       , Statement(..), Arg(..), VarDec(..), Var(..), RegAlloc(..)
       -- * Helpers to create immediate arguments
       , word8, word16, word32, word64
       -- * Constants
       , Constant(..)
       -- * Stuff with comments
       , Commented(..), (<#>), (<!>)
       -- * Some Monoids
       , Declarations, Statements, CommentMonoid

       ) where

import Data.Monoid
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
data Declaration machine where

  -- Emit verbatim text
  Verbatim :: Text -> Declaration machine

  -- Array declaration
  DArray   :: Supports machine ty
           => Array machine ty
           -> Declaration machine
  -- Function definition
  DFun     :: Function machine -> Declaration machine

instance Show (Declaration machine) where
  show (Verbatim t) = "Verbatim " ++ show t
  show (DArray   a) = "DArray ("  ++ show a ++ ")"
  show (DFun     f) = "DFun ("    ++ show f ++ ")"

-- | An array.
data Array machine ty = Array { arrayName      :: Text
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
data Scope machine = Scope { scopeParams        :: [VarDec machine]
                           , scopeLocalVars     :: [VarDec machine]
                           , scopeRegisterAlloc :: [RegAlloc machine]
                           } deriving Show

instance Monoid (Scope machine) where
  mempty  = Scope [] [] []
  mappend (Scope pa la ra) (Scope pb lb rb) =
    Scope (pa ++ pb) (la ++ lb) (ra ++ rb)

-- | An statement can take 0,1,2 or 3 arguments. The text field is the
-- neumonic of the instruction.
data Statement machine where
  -- Instruction with no operand.
  S0 :: Text -> Statement machine

  -- Instruction with one operand.
  S1 :: Supports machine ty
     => Text
     -> Arg machine ty
     -> Statement machine

  -- Two operand statement.
  S2 :: ( Supports machine ty1, Supports machine ty2 )
     => Text
     -> Arg machine ty1
     -> Arg machine ty2
     -> Statement machine
  -- Three operand statements.
  S3 :: ( Supports machine ty1
        , Supports machine ty2
        , Supports machine ty3
        )
     => Text
     -> Arg machine ty1
     -> Arg machine ty2
     -> Arg machine ty3
     -> Statement machine

instance Show (Statement machine) where
  show (S0 op      ) = unwords ["S0", show op]
  show (S1 op a    ) = unwords ["S1", show op, show a]
  show (S2 op a b  ) = unwords ["S2", show op, show a, show b]
  show (S3 op a b c) = unwords ["S2", show op, show a, show b, show c]

-- | An argument of an assembly statement.
data Arg machine ty where
  -- An immediate value
  Immediate :: Integral ty => Constant ty -> Arg machine ty

  -- A parameter. The integer denote the offset in the parmeter
  -- stack. A function having three arguments @foo@, @bar@ and @biz@
  -- will have @foo@ as parameter 0, @bar@ as parameter 1 and @biz@ as
  -- parameter 2.
  Param     :: Int -> Arg machine ty

  -- A local variable (available on the stack). A convention similar
  -- to parameter is used here.
  Local     :: Int -> Arg machine ty

  -- A machine register.
  Reg       :: (Register reg, Type reg ~ ty)
            => reg -> Arg machine ty

  -- An indirect address. The base address is stored in a register
  -- that can hold a pointer.
  Indirect  :: ( Register reg
               , MachineConstraint machine reg
               , Type reg ~ Ptr ty
               )
               => reg
               -> Int
               -> Arg machine ty

instance Show (Arg machine ty) where
  show (Immediate (Constant ty)) = show $ toInteger ty
  show (Param i)                 = "$("++ show i  ++ ")"
  show (Local i)                 = "$("++ show i  ++ ")"
  show (Reg       r)             = unpack (registerName r)
  show (Indirect r i)            = unpack (registerName r)
                                 ++ "[ " ++ show i ++ " ]"

-- | A variable declaration.
data Var ty = Var Text deriving Show

instance IsString (Var ty) where
  fromString = Var . fromString

data VarDec machine where
  VarDec :: Supports machine ty => Var ty -> VarDec machine

data RegAlloc machine where
  RegAlloc :: (Register reg, MachineConstraint machine reg)
           => reg -> RegAlloc machine

instance Show (VarDec machine) where
  show (VarDec v) = show v

instance Show (RegAlloc machine) where
  show (RegAlloc reg) = unpack $ registerName reg

------------------- Some helper functions --------------------

-- | An 8-bit unsigned integer.
word8  :: Word8  -> (Arg arch Word8)
word8  = Immediate . Constant

-- | A 16-bit unsigned integer.
word16 :: Word16 -> (Arg arch Word16)
word16 = Immediate . Constant

-- | A 32-bit unsigned integer.
word32 :: Word32 -> (Arg arch Word32)
word32 = Immediate . Constant

-- | A 64-bit unsiged integer.
word64 :: Word64 -> (Arg arch Word64)
word64 = Immediate . Constant

--------------------- Constants ----------------------------------------


-- | A constant.
data Constant ty = Constant ty


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

infix 0 <#>
infix 0 <!>
