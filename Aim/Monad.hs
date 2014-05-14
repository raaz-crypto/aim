{-|

The aim monad.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

module Aim.Monad
       ( -- * The Aim monad.
         AimT, ProgramT, doMonadic
       , Aim, Program
       , runAimT, runAim
       -- ** Function definition
       , function, param, local, register, body
       -- ** Arrays
       , array
       -- * Assembling
       , assembleM, assemble
       ) where

import Control.Applicative              ( (<$>)        )
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.String
import Data.Text                        ( Text, unpack )
import Text.PrettyPrint                 ( Doc          )

import Aim.Assembler.Internal.Language
import Aim.Assembler.Internal.Syntax
import Aim.Machine

-- | The aim monad transformer.
type AimT w machine m    = WriterT w m

-- | The aim monad.
type Aim     w machine   = AimT w machine Identity

-- | A program monad transformer.
type ProgramT machine m  = AimT (Declarations machine) machine m

-- | The program monad.
type Program   machine   = ProgramT machine Identity

-- | A block monad transformer.
type BlockT machine m    = AimT (Statements machine) machine m

-- | An function monad.
type FunctionT machine m  = StateT (Scope machine) (BlockT machine m)


-- | Do the monadic computation inside the aim monadic transformation.
doMonadic :: (Monoid w, Monad m) => m a -> AimT w machine m a
doMonadic = lift

-- | Run an aim monad to get the instructions.
runAim   :: Aim w machine a -> w
runAim   = runIdentity . runAimT

-- | Run the aim monad transformer to get the instructions.
runAimT  :: Monad m => AimT w machine m a -> m w
runAimT  = execWriterT

-- | Assemble a monadic Aim program.
assembleM :: ( Syntax syntax
            , Monad m, Functor m
            , ArchOfSyntax syntax ~ ArchOf machine
            )
         => syntax               -- ^ assembler syntax to use
         -> ProgramT machine m a -- ^ the program to assemble
         -> m Doc
assembleM syntax pg = program syntax <$> runAimT pg

-- | Assemble an Aim program.
assemble  :: ( Syntax syntax
             , ArchOfSyntax syntax ~ ArchOf machine
             )
         => syntax            -- ^ assembler syntax to use
         -> Program machine a -- ^ the program to assemble
         -> Doc
assemble syntax = program syntax <$> runAim

--------------- Function declaration ------------------------------

-- | Defining a function.
function :: Monad m
         => Text                   -- ^ function name
         -> FunctionT machine m a  -- ^ function definition
         -> ProgramT machine m ()
function fname fMonad = do
  (s, b) <- lift $ runWriterT $ execStateT fMonad mempty
  let defun = DFun Function { functionName  = fname
                            , functionScope = s
                            , functionBody  = b
                            }
      in tell [ fromString $ unpack $ "function " <> fname
              , defun <#> ("end of function " <> fname)
              ]

-- | The function body.
body :: Monad m => BlockT machine m a -> FunctionT machine m a
body = lift

-- | Parameter declaration in function.
param :: ( Supports machine ty, Monad m, Functor m)
      => Var ty
      -> FunctionT machine m (Arg machine ty)
param v = do
  p <- nParams <$> get
  modify defParam
  return $ Param p
  where defParam s = s <> mempty { scopeParams = [VarDec v] }
        nParams      = length . scopeParams

-- | Parameter declaration in function.
local :: ( Supports machine ty, Monad m, Functor m)
      => Var ty
      -> FunctionT machine m (Arg machine ty)
local v = do
  l <- nLocal <$> get
  modify defLocal
  return $ Param l
  where defLocal s = s <> mempty { scopeLocalVars = [VarDec v] }
        nLocal       = length . scopeLocalVars

register :: ( Register reg
            , MachineConstraint machine reg
            , Type reg ~ ty
            , Monad m
            )
            => reg
            -> FunctionT machine m (Arg machine ty)
register r = do
  modify allocReg
  return $ Reg r
  where allocReg s = s <> mempty { scopeRegisterAlloc = [ RegAlloc r ]}

------------------------ Array declaration --------------------------

array :: ( Supports machine ty
         , Integral ty
         , Monad m
         )
      => Text
      -> [ty]
      -> ProgramT machine m ()
array aName tys = tell [ a <#> "Array " <> aName ]
  where a = DArray $ mkArray tys
        mkArray :: Integral t => [t] -> Array machine t
        mkArray ts = Array { arrayName     = aName
                           , arrayContents = map toInteger ts
                           }
