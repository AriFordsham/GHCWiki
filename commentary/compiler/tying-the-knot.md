# Tying the knot


Background reading: [ GHC at The Architecture of Open Source Applications](http://www.aosabook.org/en/ghc.html) (search for "No Symbol Table").


Compilers usually have one or more data structures known as *symbol tables*, which are mappings from symbols to information about the symbol in question. GHC avoids symbol tables; instead, a symbol *contains* all information about itself. Thus, the data types for [Haskell entities](commentary/compiler/entity-types) (Id, TyVar, TyCon, DataCon, and Class) form an immutable cyclic data structure, where everything points to everything else. This makes it very convenient for the consumer, because there are accessor functions with simple types, such as `idType :: Id -> Type`.


The downside of these cyclic data structures is that they are difficult to update.  This has two implications: (1) we have to construct this graph in one go using a technique called *tying the knot* (since we can't update the graph after the fact--it's immutable!) and (2) if any of the data in the graph ever becomes out-of-date (as can occur when typechecking hs-boot loops), we have to throw out all of the in-memory data structures and rebuild the graph from scratch.  How this knot tying works is a dark corner of GHC, but hopefully this wiki page will shed some light on the matter.

## Graph representation versus interface representation


Most type-checker entities which form the graph representation have a sister representation which is not cyclic, uses symbol tables, and suitable for serialization into an interface file.  Here are some of the main correspondences:

```wiki
Interface           Graph
---------------------------------------------
data ModIface       data ModDetails
data IfaceDecl      data TyThing
  = IfaceData       data TyCon = AlgTyCon
  | IfaceSynonym               | SynonymTyCon
  | IfaceFamily                | FamilyTyCon
  | IfaceClass      data Class
  | IfaceId         data Id
  | IfaceAxiom      data CoAxiom
  | IfacePatSyn     data PatSyn
data IfaceConDecl   data DataCon
data IfaceType      data Type
data IfaceExpr      data CoreExpr
data IfaceCoercion  data Coercion
data IfaceClsInst   data ClsInst
data IfaceFamInst   data FamInst
data IfaceRule      data CoreRule
```


Taking `IfaceType` and `Type` as an example, we can see the big difference in a constructor for type constructor application:

```wiki
data Type
  = ...
  | TyConApp TyCon [KindOrType]

data IfaceType
  = ...
  | IfaceTyConApp IfaceTyCon IfaceTcArgs
data IfaceTyCon
  = IfaceTyCon { ifaceTyConName :: IfExtName
               , ifaceTyConInfo :: IfaceTyConInfo }       
```


In `Type`, the type constructor application contains the full `TyCon` which contains everything we could possibly want to know about the type constructor (e.g., if it is a synonym, what its unfolding is). In `IfaceType`, the application points to a stub data structure `IfaceTyCon` which only records the `IfExtName` of the `TyCon` in question (which is just a `Name`); to find out more information, we will have to go out and lookup this `IfExtName` in the symbol table associated with the interface type.

**Converting from graph to interface representation.**  The primary functions which turn `ModDetails` into `ModIface` are `mkIface` (when we generated code) and `mkIfaceTc` (when we did not generate code.) There is also `tyThingToIfaceDecl` (which does the obvious thing) and `toIface*****` functions. These functions are all relatively straightforward: since all graph representations record globally unique `Name`s identifying them, all we need to do is drop the extra information and preserve only the `Name`.

**Converting from interface to graph representation.** This process is referred to as *typechecking the interface* (in `TcIface`).  Unlike the conversion to interface format, which is essentially pure, conversion from the interface format involves some global state: the existing graph of type checking entities which we are going to resolve `Name` references to. Generally speaking, there is a \*unique\* such graph, such that every `Name` maps to a unique `TyThing` in the graph, spanning over all of the typechecked entities GHC could possibly know about. In the common case, the only reason we typecheck an interface is to (lazily) load its entities into this global map.
