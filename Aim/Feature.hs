{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-|

Module that captures features. One of the important consideration in
the design of Aim is that errors that occur by the mixing of
incompatible instructions has to be reported at compile time rather
than at runtime. This requires a systematic way to constraint
assembler instructions depending on stuff like architecture, specific
features that might be supported by the architecture (e.g.  SSE). This
module facilitates capturing such constraints at the type level.

The constraints that we support are conjunctions of what are instances
of the class `Feature`. The type class `:=>:` then captures the
implication relation on conjunctions of features.

-}

module Aim.Feature
       ( Feature
       , (:=>:)
       , (:/\:)
       ) where

-- | A class that captures all basic features. Example for features
-- are assertions like the architecture is X86 or something similar.
class Feature feature where

-- | A conjunction type. This is used to capture a conjunction of
-- features.
data feature :/\: conjunction

-- | A constraint @conjA `:=>:` conjB@ captures implies relation among
-- a conjuctions @conjA@ and @conjB@. Assume that we have already
-- defined a feature instance for the types @Arch X86@ and @SSE@. We
-- can use the `:=>:` to /restrict/ (at the type level) the
-- instruction @instr@ to all X86 architecture with support for SSE as
-- follows:
--
-- > instr :: arch :=>: Arch X86 :/\: SSE
-- >       => ...
-- >
class conjA :=>: conjB where

infixr 9 :/\:
infixr 8 :=>:

instance Feature feature => feature :=>: feature where

instance Feature feature => feature :/\: conj :=>: feature where

instance ( Feature feature
         , conjA :=>: conjB
         , conjA :=>: feature
         )
         =>
         conjA :=>: feature :/\: conjB where
