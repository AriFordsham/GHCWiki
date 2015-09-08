# The `SignatureSections` Language Extension

## Semantics


The `SignatureSections` language pragma enables the use of the `::` type-signature operator in "type-signature sections" expressions, i.e.

> *aexp* → `(``::`*type*`)`


where *type* must not have any free (not bound by outer context) type-variables.

**Translation:** The following identity holds

> `(``::`*type*`)` = `\`*x*`->`*x*`::`*type*

## Applications


Like Haskell2010 expression type-signatures, type-signature sections are useful to explicitly type expressions to help resolve ambiguous typing (e.g. due to overloading).

### Examples

```
canonDouble::String->StringcanonDouble= show . read                -- type errorcanonDouble= show .(::Double). read  -- OK
```

```
typeRep:: proxy a ->TypeRepdataProxy a =Proxy-- without `SignatureSections`a= typeRep (Proxy::ProxyBool)-- with `SignatureSectionsa= typeRep (::Bool)
```

### References

- [\#10803](https://gitlab.haskell.org//ghc/ghc/issues/10803)
- [ http://augustss.blogspot.co.at/2014/04/a-small-haskell-extension.html](http://augustss.blogspot.co.at/2014/04/a-small-haskell-extension.html)