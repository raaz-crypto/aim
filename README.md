
# AIM: A DSL to generate assembly language.

Aim (Assembler In a Monad) is a DSL implemented in Haskell to generate
assembly language. It is largely inspired by [Daniel Bernstein][djb]'s
[qhasm]. However, implementing it as a DSL would give the entire power
of Haskell at our disposal. For example, macros are already "builtin":
one just uses a Haskell function to generate the code.

[djb]: <http://cr.yp.to/djb.html>     "Daniel Bernstein"
[qhasm]: <http://cr.yp.to/qhasm.html> "Qhasm"
