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
of the class `Feature`. The type class `Supports` then captures the
membership relation on conjunctions of features.

-}

module Aim.Feature
       ( Feature
       , Supports
       , (:/\:)
       ) where

-- | A class that captures all basic features. Example for features
-- are assertions like hasAESNI or SSE or something similar.
class Feature feature

-- | A conjunction type. This is used to capture a conjunction of
-- features.
data feature :/\: conjunction

-- | A constraint @Supports conj feature@ captures the membership relation
-- among a set of features(represented by @conj@) and a @feature@.
class (Feature feature) => Supports conj feature

instance Feature feature => Supports feature feature

instance (Feature feature, Supports conjA feature) => Supports (conjA :/\: conjB) feature
