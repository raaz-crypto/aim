{-|

The aim monad.


-}

{-# LANGUAGE TypeSynonymInstances #-}
module Aim.Monad
       ( AimT, ProgramT, BlockT
       , Aim, Program, Block
       , AsmBlock, AsmProgram
       , doMonadic
       , runAimT, runAim

       ) where

import Control.Monad.Writer
import Control.Monad.Identity

import Aim.Assembler.Language
import Aim.Machine

type AsmBlock   machine = BlockMonoid     (ArchOf machine)
type AsmProgram machine = ProgramMonoid   (ArchOf machine)

-- | The aim monad transformer.
type AimT w machine m    = WriterT w m

-- | A program monad transformer.
type ProgramT machine m  = AimT (AsmProgram machine) machine m

-- | A block monad transformer.
type BlockT machine m    = AimT (AsmBlock machine) machine m

-- | The aim monad.
type Aim     w machine   = AimT w machine Identity

-- | The program monad.
type Program   machine   = ProgramT machine Identity

-- | The block monad.
type Block     machine   = BlockT machine Identity

-- | Do the monadic computation inside the aim monadic transformation.
doMonadic :: (Monoid w, Monad m) => m a -> AimT w machine m a
doMonadic = lift


-- | Run an aim monad to get the instructions.
runAim   :: Aim w machine a -> w
runAim   = runIdentity . runAimT

-- | Run the aim monad transformer to get the instructions.
runAimT  :: Monad m => AimT w machine m a -> m w
runAimT  = execWriterT
