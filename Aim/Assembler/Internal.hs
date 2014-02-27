{-|

This exposes the internals of the @aim@. The stuff that is exposeds
here is an internal (mostly type less) abstract syntax for assembly
language. To take maximum advantage of type safety an @aim@ user
should avoid using these internals directly. This is mainly meant for
developers who:

1. Want to add extra architectures

2. Want to add extra assembler syntax.


-}

module Aim.Assembler.Internal
       ( module Aim.Assembler.Internal.Language
       , module Aim.Assembler.Internal.Syntax
       ) where

import Aim.Assembler.Internal.Language
import Aim.Assembler.Internal.Syntax
