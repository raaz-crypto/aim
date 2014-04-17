{-|

This module captures various assembler syntaxes. The same
archictecture can have multiple syntax: for example the X86
architecture has both the GNU as well as the INTEL syntax.  The class
here supports defining different syntaxes.

-}

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
module Aim.Assembler.Internal.Syntax
       ( Syntax(..)
       --
       -- * Helper function for commenting
       -- ** Line comments
       --
       , lineCommentPrefix
       , lineCommentShell
       , lineCommentLisp
       , lineCommentHaskell
       , lineCommentCPP
       --
       -- ** Block comments
       --
       , commentBlock
       , commentShell
       , commentLisp
       , commentHaskell
       , commentC
       ) where

import Data.Monoid
import Data.Text   ( Text, lines, unlines, intercalate )
import Prelude     ( map, ($), (.)                     )


import Aim.Machine( Arch )
import Aim.Assembler.Internal.Language


-- | This class captures when an architecture has a given syntax. For
-- example, if we have an instance of @`Syntax` Foo MyArch@ then it
-- means that the @Foo@ assembly syntax supports the architecture
-- @MyArch@. We can then convert a program for the architecture
-- @MyArch@ to text.

class Arch (ArchOfSyntax syntax) => Syntax syntax where

  -- | The architecture for which this is a syntax.
  type ArchOfSyntax syntax

  -- | Textual reprensentation `Arg`.
  arg          :: syntax
               -> Stack
               -> Arg (ArchOfSyntax syntax)
               -> Text

  -- | Textual representation of an instruction.
  instruction  :: syntax
               -> Text   -- ^ opcode.
               -> [Text] -- ^ textual representation of the operand.
               -> Text

  -- | Textual representation of a statement. If instruction is
  -- defined then there is a default definition for this member.
  statement    :: syntax
               -> Stack
               -> Statement (ArchOfSyntax syntax)
               -> Text
  statement syn _  (S0 opc      ) = instruction syn opc []
  statement syn st (S1 opc a    ) = instruction syn opc [ arg syn st a ]
  statement syn st (S2 opc a b  ) = instruction syn opc
                                    [ arg syn st a
                                    , arg syn st b
                                    ]
  statement syn st (S3 opc a b c) = instruction syn opc
                                    [ arg syn st a
                                    , arg syn st b
                                    , arg syn st c
                                    ]

  -- | Textual representation of and array declaration
  declareArray :: syntax
               -> Array (ArchOfSyntax syntax)
               -> Text

  -- Textual representation of a function definition
  declareFunction :: syntax
                  -> Text     -- Function name
                  -> Stack
                  -> BlockMonoid (ArchOfSyntax syntax)


  -- | Textual representation of a definition.
  declaration :: syntax
             -> Declaration (ArchOfSyntax syntax)
             -> Text

-- | How to comment a line of the program.
  commentLine :: syntax
              -> Text -- ^ program line
              -> Text -- ^ comment.
              -> Text

  -- | How write a block comment.
  blockComment :: syntax -- ^ The syntax
               -> [Text] -- ^ block comment
               -> Text

-- | This function can be used to comment when the underlying language
-- supports line comments that start with a prefix. For example if the
-- language supports shellLike comments use @prefixWith "#"@.
lineCommentPrefix :: Text  -- ^ Line comment starting prefix
                  -> Text  -- ^ The actual comment
                  -> Text
lineCommentPrefix start txt = intercalate "\n"
                            $ map singleLine
                            $ lines txt
  where singleLine com = start <> " " <> com


-- | Line commenting Shell style
lineCommentShell :: Text -> Text
lineCommentShell = lineCommentPrefix "#"

-- | Line commenting Lisp style
lineCommentLisp :: Text -> Text
lineCommentLisp = lineCommentPrefix ";"

-- | Line commenting Haskell style
lineCommentHaskell :: Text -> Text
lineCommentHaskell = lineCommentPrefix "--"

-- | Line commenting C++     style
lineCommentCPP :: Text -> Text

lineCommentCPP = lineCommentPrefix "//"

-- | Generate a block comment.
commentBlock :: Text -- ^ Comment start
             -> Text -- ^ Comment line start
             -> Text -- ^ Comment end
             -> Text -- ^ Comment.
             -> Text
commentBlock start line end txt = unlines [ start
                                          , lineCommentPrefix line txt
                                          , end
                                          ]

-- | Block commenting Shell style
commentShell :: Text -> Text
commentShell = commentBlock "##"  "##" "##"

-- | Block commenting Lisp style
commentLisp :: Text -> Text
commentLisp = commentBlock ";;"  ";;" ";;"

-- | Block commenting Haskell style
commentHaskell :: Text -> Text
commentHaskell = commentBlock "{-" "  " "-}"

-- | Block commenting C       style
commentC :: Text -> Text
commentC = commentBlock "/*" "**" "*/"
