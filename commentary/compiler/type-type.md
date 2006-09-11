# The data type `Type` and its friends


GHC's compiles a typed programming lanuage, and GHC's intermediate language is explicitly typed.  So the data type that GHC uses to represent types is of central importance.


The first thing to realise is that GHC uses a *single* data type for types, even though there are two different "views".  

- The "typechecker view" (or "source view") regards the type as a Haskell type, complete with implicit parameters, class constraints, and the like.  For example:

  ```wiki
    forall a. (Eq a, %x::Int) => a -> Int
  ```
- The "core view" regards the type as a Core-language type, where class and implicit parameter constraints are treated as function arguments:

  ```wiki
    forall a. Eq a -> Int -> a -> Int
  ```


These two "views" are supported by a family of functions operating over that view:

- [compiler/types/TypeRep.lhs](/trac/ghc/browser/ghc/compiler/types/TypeRep.lhs): here is where `Type` is defined.
- [compiler/types/Type.lhs](/trac/ghc/browser/ghc/compiler/types/Type.lhs): core-view utility functions over `Type`.
- [compiler/typecheck/TcType.lhs](/trac/ghc/browser/ghc/compiler/typecheck/TcType.lhs): source-view utility functions over `Type`.


The module `TypeRep` exposes the representation becauese a few other modules (`Type`, `TcType`, `Unify`, etc) work directly on its representation.  However, you should not lightly pattern-match on `Type`; it is meant to be an abstract type.  Instead, try to use functions defined by `Type`, `TcType` etc.


The single data type `Type` is used to represent

- Types (possibly of higher kind); e.g. `[Int]`, `Maybe`
- Coercions; e.g. `trans (sym g) h`
- Kinds (which classify types and coercions); e.g. `(* -> *)`, `T :=: [Int]`
- Sorts (which classify types); e.g. `TY`, `CO`


GHC's use of [coercions and equality constraints](commentary/compiler/fc) is important enough to deserve its own page.

## The representation of `Type`


Here, then is the representation of types (see [compiler/types/TypeRep.lhs](/trac/ghc/browser/ghc/compiler/types/TypeRep.lhs) for more details):

```wiki
data Type = TyVarTy TyVar			-- Type variable
  	  | AppTy Type Type			-- Application
  	  | TyConApp TyCon [Type]		-- Type constructor application
  	  | FunTy Type Type			-- Arrow type
  	  | ForAllTy TyVar Type			-- Polymorphic type
  	  | PredTy PredType			-- Type constraint
  	  | NoteTy TyNote Type			-- Annotation

data PredType = ClassP Class [Type]		-- Class predicate
              | IParam (IPName Name) Type	-- Implicit parameter
              | EqPred Type Type		-- Equality predicate (ty1 :=: ty2)

data TyNote = FTVNote TyVarSet	-- The free type variables of the noted expression
```

## Kinds


Kinds are represented as types:

```wiki
type Kind = Type
```


Basic kinds are now
represented using type constructors, e.g. the kind `*` is represented as

```wiki
liftedTypeKind :: Kind
liftedTypeKind = TyConApp liftedTypeKindTyCon []
```


where `liftedTypeKindTyCon` is a built-in `PrimTyCon`.  The arrow type
constructor is used as the arrow kind constructor, e.g. the kind `* ->*` 
is represented internally as

```wiki
FunTy liftedTypeKind liftedTypeKind
```


It's easy to extract the kind of a type, or the sort of a kind:

```wiki
typeKind :: Type -> Kind
```


The "sort" of a kind is always one of the
sorts: `TY` (for kinds that classify normal types) or `CO` (for kinds that
classify coercion evidence).  The coercion kind, `T1 :=: T2`, is
represented by `PredTy (EqPred T1 T2)`.

## Type variables


Type variables are represented by the `TyVar` constructor of the [data type Var](commentary/compiler/entity-types).  


Type variables range over both *types* (possibly of higher kind) or *coercions*.  You could tell the differnece between these two by taking the `typeKind` of the kind of the type variable, adn seeing if you have sort `TY` or `CO`, but for efficiency the `TyVar` keeps a boolean flag, and offes a function:

```wiki
  isCoercionVar :: TyVar -> Bool
```

## Classifying types


GHC uses the following nomenclature for types:

<table><tr><th>**Unboxed**</th>
<td>A type is unboxed iff its representation is other than a pointer. Unboxed types are also unlifted.
</td></tr></table>

<table><tr><th>**Lifted**</th>
<td>A type is lifted iff it has bottom as an element. Closures always have lifted types:  i.e. any let-bound identifier in Core must have a lifted type.  Operationally, a lifted object is one that can be entered. Only lifted types may be unified with a type variable.
</td></tr></table>

<table><tr><th>**Data**</th>
<td>A type declared with **`data`**.  Also boxed tuples.
</td></tr></table>

<table><tr><th>**Algebraic**</th>
<td>An algebraic data type is a data type with one or more constructors, whether declared with `data` or `newtype`.   An algebraic type is one that can be deconstructed        with a case expression.  "Algebraic" is **NOT** the same as "lifted",  because unboxed tuples count as "algebraic".
</td></tr></table>

<table><tr><th>**Primitive**</th>
<td>a type is primitive iff it is a built-in type that can't be expressed        in Haskell.
  
Currently, all primitive types are unlifted, but that's not necessarily the case.  (E.g. Int could be primitive.)
</td></tr></table>

>
> Some primitive types are unboxed, such as Int\#, whereas some are boxed but unlifted (such as ByteArray\#).  The only primitive types that we classify as algebraic are the unboxed tuples.


Examples of type classifications:

<table><tr><th></th>
<th>**Primitive**</th>
<th>**Boxed**</th>
<th>**Lifted**</th>
<th>**Algebraic**</th></tr>
<tr><th>`Int#`</th>
<th> Yes             </th>
<th> No        </th>
<th> No          </th>
<th> No                
</th></tr>
<tr><th>`ByteArray#`</th>
<th> Yes             </th>
<th> Yes        </th>
<th> No          </th>
<th> No                
</th></tr>
<tr><th>`(# a, b #)`</th>
<th> Yes             </th>
<th> No        </th>
<th> No          </th>
<th> Yes        
</th></tr>
<tr><th>`(  a, b  )`</th>
<th> No             </th>
<th> Yes        </th>
<th> Yes          </th>
<th> Yes        
</th></tr>
<tr><th>`[a]`</th>
<th> No             </th>
<th> Yes        </th>
<th> Yes          </th>
<th> Yes        
</th></tr></table>