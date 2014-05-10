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
       , program
       , Pretty(..)
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
import Data.Maybe                  ( maybe               )
import Data.Text                   ( Text, unpack, lines )
import Prelude                     ( map, ($), (.)       )
import Text.PrettyPrint

import Aim.Machine                 ( Arch, Machine(..)   )
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
  arg          :: (ArchOf machine ~ ArchOfSyntax syntax)
               => syntax
               -> Stack
               -> Arg machine
               -> Doc

  -- | Textual representation of an instruction.
  instruction  :: syntax
               -> Text   -- ^ opcode.
               -> [Doc] -- ^ textual representation of the operand.
               -> Doc

  -- | Textual representation of a statement. If instruction is
  -- defined then there is a default definition for this member.
  statement    :: (ArchOf machine ~ ArchOfSyntax syntax)
               => syntax
               -> Stack
               -> Statement machine
               -> Doc
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
  declareArray :: (ArchOf machine ~ ArchOfSyntax syntax)
               => syntax
               -> Array machine
               -> Doc

  -- | Textual representation of a function definition
  declareFunction :: syntax
                  -> Text     -- Function name
                  -> Stack
                  -> Doc      -- Body.
                  -> Doc

  -- | Textual representation of a definition.
  declaration :: (ArchOf machine ~ ArchOfSyntax syntax)
              => syntax
              -> Declaration (ArchOfSyntax syntax)
              -> Doc

  -- | How to comment a line of the program.
  commentLine :: syntax
              -> Doc  -- ^ program line
              -> Text -- ^ comment.
              -> Doc

  -- | How write a block comment.
  blockComment :: syntax -- ^ The syntax
               -> Doc    -- ^ block comment
               -> Text
               -> Doc


------------------------ Pretty printing a program -----------------

-- | Pretty prints a program according to a given syntax.
program :: (Syntax syntax, ArchOf machine ~ ArchOfSyntax syntax)
        => syntax
        -> Declarations machine
        -> Doc
program syn = commenter dec (blockComment syn)
  where blockDoc stack = commenter
                         (statement syn stack)
                         (commentLine syn)
        dec (Verbatim txt) = doc txt
        dec (DArray arr  ) = declareArray syn arr
        dec (DFun f      ) = declareFunction syn (functionName  f)
                                                 stack
                                                 body
          where stack   = functionStack f
                body    = blockDoc stack (functionBody f)

-- | Given a comment monoid converts it to a `Doc`. You need to give a
-- way to pretty print the content type and a way to convert each
-- commented entry to the appropriate doc.
commenter :: (a   -> Doc)          -- ^ pretty printer for the content
                                   -- type
          -> (Doc -> Text -> Doc)  -- ^ commenting individual content.
          -> CommentMonoid a
          -> Doc
commenter prettya com = vcat . map mapper
  where mapper (Comment ma txt) = com adoc txt
          where adoc = maybe empty prettya ma

-------------------- Pretty printing ------------------------------

class Pretty a where
  doc :: a -> Doc

instance Pretty Text where
  doc = text . unpack

------------------- Some commenting styles -------------------------

-- | This function can be used to comment when the underlying language
-- supports line comments that start with a prefix. For example if the
-- language supports shellLike comments use @prefixWith "#"@.
lineCommentPrefix :: Doc   -- ^ Line comment starting prefix
                  -> Text  -- ^ The actual comment
                  -> Doc
lineCommentPrefix start txt = vcat
                            $ map singleLine
                            $ lines txt
  where singleLine com = start <+> doc com


-- | Generate a block comment.
commentBlock :: Doc -- ^ Comment start
             -> Doc -- ^ line start
             -> Doc -- ^ Comment end
             -> Text -- ^ Comment.
             -> Doc
commentBlock start line end txt =
  start
  $+$ lineCommentPrefix line txt
  $+$ end

-- | Line commenting Shell style
lineCommentShell :: Text -> Doc
lineCommentShell = lineCommentPrefix "#"

-- | Line commenting Lisp style
lineCommentLisp :: Text -> Doc
lineCommentLisp = lineCommentPrefix ";"

-- | Line commenting Haskell style
lineCommentHaskell :: Text -> Doc
lineCommentHaskell = lineCommentPrefix "--"

-- | Line commenting C++     style
lineCommentCPP :: Text -> Doc
lineCommentCPP = lineCommentPrefix "//"



-- | Block commenting Shell style
commentShell :: Text -> Doc
commentShell = commentBlock "##"  "##" "##"

-- | Block commenting Lisp style
commentLisp :: Text -> Doc
commentLisp = commentBlock ";;"  ";;" ";;"

-- | Block commenting Haskell style
commentHaskell :: Text -> Doc
commentHaskell = commentBlock "{-" "  " "-}"

-- | Block commenting C       style
commentC :: Text -> Doc
commentC = commentBlock "/*" "**" "*/"
