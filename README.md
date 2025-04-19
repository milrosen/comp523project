run with 
`dune exec MacrosProject ./bin/example.badmacros`

# An implementation of the paper "Taming Macros" for Comp 523. 

This toy language implements a very paired down version of macros (no code execution, expansion before eval), with gaurantees that macro expansion maintains preservation and progress (ie, the expansion of any macro will never generate code that another macro isn't expecting)

The "shape" checker is also powerfull enough to essentially replace the parser, in that we can add each of the keywords as "macros" and be assured that our code is also using each of the primitives correctly. However, it is also the fact that we are forcing ourselves to do all this work before parsing that prevents us from having macros which execute code.

The basic idea is that we assign a "shape" to each expression. For instance, `moo (a b) (c d)` is an `ident (ident ident) (ident ident)` (unless `moo` is a macro). Next, we define a sub-shape relation. This says (among other things) that every `ident` is also an `expr` and that every `(expr expr)` is also an `expr`. Once we have our subshaping relationship, it's not that hard to simply directly annotate each macro with a shape. (for instance, we might want `moo` to be a `(ident ident) expr -> expr`). Then, we would know that each macro is sucessfully applied when its the arguments are a subshape of the template.

For macros  with multiple guards, we just need to make sure that it's never ambiguous which one matches, so we impose an additional restriction: if we assign the top `any` shape to every variable in a pattern, that no patterns are subtypes of each other. This is also a pretty heavy restriction, since `(ident ident)` is a subtype of `any.` Effectively, this restriction forces each pattern to have a different lenght of terms, or different nesting within each of those terms, no matter how different the shapes of the variables are.  
