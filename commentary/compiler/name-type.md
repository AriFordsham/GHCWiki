# The `Name` type


Every entity (type constructor, class, identifier, type variable) has a `Name`. The Name type is pervasive in GHC, and is defined in [compiler/basicTypes/Name.hs](/ghc/ghc/tree/master/ghc/compiler/basicTypes/Name.hs). Here is what a `Name` looks like, though it is private to the Name module:

```wiki
data Name = Name {
	      n_sort :: NameSort,	-- What sort of name it is
	      n_occ  :: !OccName,	-- Its occurrence name
	      n_uniq :: Int#,		-- Its identity
	      n_loc  :: !SrcLoc		-- Definition site
	  }
```

- The `n_sort` field says what sort of name this is: see **[NameSort](commentary/compiler/name-type#the-namesort-of-a-name)** below. 
- The `n_occ` field gives the "occurrence name", or **[OccName](commentary/compiler/rdr-name-type#the-occname-type)**, of the Name.
- The `n_uniq` field allows fast tests for equality of Names. 
- The `n_loc` field gives some indication of where the name was bound. 

## The `NameSort` of a Name


There are four flavours of Name: 

```wiki
data NameSort
  = External Module (Maybe Name)
	-- (Just parent) => this Name is a subordinate name of 'parent'
	-- e.g. data constructor of a data type, method of a class
	-- Nothing => not a subordinate
 
  | WiredIn Module (Maybe Name) TyThing BuiltInSyntax
	-- A variant of External, for wired-in things

  | Internal		-- A user-defined Id or TyVar
			-- defined in the module being compiled

  | System		-- A system-defined Id or TyVar.  Typically the
			-- OccName is very uninformative (like 's')
```

<table><tr><th><tt>Internal</tt>, <tt>System</tt></th>
<td>
An <tt>Internal</tt> <tt>Name</tt> has only an occurrence name. Distinct <tt>Internal</tt> <tt>Names</tt> may have the same occurrence name; the <tt>n_uniq</tt> distinguishes them.  
</td></tr></table>


>
>
> There is only a tiny difference between `Internal` and `System`; the former simply remembers that the name was originally written by the programmer, which helps when generating error messages.
>
>

<table><tr><th><tt>External</tt></th>
<td>
An <tt>External</tt> <tt>Name</tt> has a globally-unique (module, occurrence name) pair, namely the original name of the entity, that describes where the thing was originally defined. So for example, if we have 

```wiki
module M where
  f = e1
  g = e2

module A where
  import qualified M as Q
  import M
  a = Q.f + g
```

then in module <tt>A</tt>, the function <tt>Q.f</tt> has an External Name <tt>M.f</tt>.
</td></tr></table>


>
> >
> >
> > During any invocation of GHC, each (module, occurrence-name) gets one, and only one, `Unique`, stored in the `n_uniq` field of the `Name`.  This association remains fixed even when GHC finishes one module and starts to compile another.  This association between (module, occurrence-name) pairs and the corresponding `Name` (with its `n_uniq` field) is maintained by the Name Cache.
> >
> >
>

<table><tr><th><tt>WiredIn</tt></th>
<td>
A <tt>WiredIn</tt> <tt>Name</tt> is a special sort of <tt>External</tt> <tt>Name</tt>, one that is completely known to the compiler (e.g. the <tt>Bool</tt> type constructor).  See <a href="commentary/compiler/wired-in">Commentary/Compiler/WiredIn</a>.
</td></tr></table>


>
> >
> >
> > The `BuiltInSyntax` field is just a boolean yes/no flag that identifies entities that are denoted by built-in syntax, such as `[]` for the empty list.  These `Names` aren't "in scope" as such, and we occasionally need to know that.
> >
> >
>

## Entities and `Names`


Here are the sorts of Name an entity can have: 

- Class: always has an `External` Name. 

- TyCon: always has an `External` or `WiredIn` Name. 

- TyVar: can have `Internal`, or `System` Names; the former are ones arise from instantiating programmer-written type signatures.

- Ids: can have `External`, `Internal`, or `System` Names. 

  - Before CoreTidy, the Ids that were defined at top level in the original source program get `External` Names, whereas extra top-level bindings generated (say) by the type checker get `Internal` Names. This distinction is occasionally useful for filtering diagnostic output; e.g. for `-ddump-types`. 
  - After CoreTidy: An Id with an `External` Name will generate symbols that appear as external symbols in the object file. An Id with an `Internal` Name cannot be referenced from outside the module, and so generates a local symbol in the object file. The CoreTidy pass makes the decision about which names should be External and which Internal. 
