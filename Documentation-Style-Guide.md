This document is a living, incremental style guide targeted at contributors to the `base` library's documentation. Its first and main purpose is to standardise our current practices when it comes to documentation writing.

This guide is not a language guide on the English language. The writer should refer to the New Oxford Style Manual, the Chicago Manual of Style, or any other professional reference for any concerns pertaining to writing in the English language.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
"OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

## Table of contents

[[_TOC_]]

## British vs. American spelling

Being an international community of contributors, many of whom coming from English-speaking countries, there are bound to be different spelling standards living with each-other.  

* If editing an existing document, consistency must be favoured. If the document is inconsistent, an arbitrary spelling can be picked in order to harmonise the document.
* When creating a new document, the kind of spelling should be the one the author(s) are most familiar with, as to avoid potential irregularities.

## Greek letters for types, Latin letters for terms

The "Greek letters for types, Latin letters for terms" (GLT,LLT) convention, whilst in use in some circles, is not as widespread as one may think. In particular, it can be harder for the reader to deal with another alphabet if Latin is not their first one.

GLT,LLT can be used in a document, under those conditions:

* The document must contain right after its introductory or summary paragraph a list of the Greek letters used below and their Latin names.
* The use of GLT,LLT must be consistent throughout the document.

Current usage has :

| Greek letter | Latin name | Usage / Position | Example       |
| ------------ | ---------- | ---------------- | ------------- |
| α            | Alpha      | Concrete type    | id :: α -> α  |
| β            | Beta       | Concrete type    | swap :: (α, β) -> (β, α)
| τ            | Tau        | Functor τ        | foldr  :: Foldable τ => (α -> β -> β) -> β -> τ α -> β

Due to its usage and history, λ (Lambda) should be avoided in order to prevent confusion.

For ease of input, Greek letters can be written as HTML codes if the section that holds them will not be rendered outside of a browser. Introductory / summary paragraphs are expected to be rendered in non-HTML environments such as GHCi, and thus should contain Unicode characters instead of their HTML code counterparts.

## Module documentation

Module documentation should contain an `Overview` section that describes in greater length the purpose, APIs and underlying concepts exposed by the module.
This section is typically not rendered through GHCi's `:doc` due to the particularities of a REPL's environment. That is why modules should always have a smaller, more compact introductory / summary paragraph that will be the first point of contact with the developer when they look modules up in the REPL.

## Comments

There are two kinds of comments in source code, comments that describe the interface (i.e. how is this supposed to be used) and comments that describe the implementation (e.g. subtle gotchas). 


### Comments on top-level entities

Every top-level entity should have a Haddock comment that describes what it does and, if needed, why it's there. Example:

```haskell
-- | Returns which registers are read and written by this 
-- instruction, as a (read, written) pair. This info is used
-- by the register allocator.
x86_regUsageOfInstr :: Platform -> Instr -> RegUsage
```

### Comments in the source code

Commenting is good but

- long comments *interleaved with the code* can make the code itself incredibly hard to read, and
- long comments *detached from the code* are easy to miss when you are editing the code itself, and soon become out of date or even misleading.


A consensus was reached on the use of `Note` blocks outside of the code they document, with a reference to them at the appropriate places.  
For example:  

```haskell
prepareRhs :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs env (Cast rhs co)    -- Note [Float coercions]
  | (ty1, _ty2) <- coercionKind co      -- Do *not* do this if rhs is unlifted 
  , not (isUnLiftedType ty1)            -- see Note [Float coercions (unlifted)]
  = do  { (env', rhs') <- makeTrivial env rhs
        ; return (env', Cast rhs' co) }

        ...more equations for prepareRhs....

{- Note [Float coercions]
   ~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  
        ...more stuff about coercion floating...
-}
```


Notice that

- **Interleaved with the code** is a short link `Note [Float coercions]`. You can't miss it when you are editing the code, but you can still see the code itself.
- **Detached from the code** is the linked comment, starting with the same string `Note [Float coercions]`.  It can be long, and often includes examples.


The standard format "`Note [Float coercions]`" serves like an URL, to point to an out-of-line comment.  Usually the target is in the same module, but not always.  Sometimes we say

```haskell
    -- See Note [Float coercions] in SpecConstr.lhs
```


Please use this technique.  It's robust, and survives successive changes to the same lines of code.  When you are changing code, it draws attention to non-obvious things you might want to bear in mind.  When you encounter the note itself you can search for the string to find the code that implements the thoughts contained in the comment.

### Comments and examples


When writing a comment to explain a subtle point, consider including an example code
snippet that illustrates the point.  For example, the above `Note [Float coercions]` continues thus:

```wiki
There's a chance that be will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          x = T m
          go 0 = 0
          go n = case x of { T m -> go (n-m) }
                -- This case should optimise
```


These kind of code snippets are extremely helpful to illustrate the point in a
concrete way.  Other ways of making the comment concrete are:

- Cite a particular ticket that this bit of code deals with
- Cite a test case in the test suite that illustrates it

### Longer comments or architectural commentary


Comments with a broad scope, describing the architecture or workings of more than one module, belong here in the commentary rather than in the code.  Put the URL for the relevant commentary page in a comment in the code itself, and also put URLs for all relevant commentary pages in a comment at the top of each module.


## Sources

### Papers

References to published papers should contain a direct URL to the document, or if unavailable, to the paper's page on the publisher website. It is discouraged to reference papers that are behind paywalls. The Digital Object Identifier (DOI) may be given as well in order to enable the reader's ability to find the paper online should the provided URL suffer from bitrot.