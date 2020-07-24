Following the high-level plan posted in [ghc proposal#306](https://github.com/ghc-proposals/ghc-proposals/pull/306), we have been planning on switching our error representation away from mere documents. The end goal is to have subsystem-specific error data types, where each constructor of such a data type will represent one error that this subsystem can throw. A global `GhcError` sum type would also be provided, at the driver level, so as to be able to represent any error thrown by any subsystem and provide a trivial mechanism for API users to deal with error values (namely: just hand them a bag of `GhcError` values). Below is the current plan for getting there.

<details><summary>Background: the current error infrastructure (click the arrow to expand!)</summary>

We currently have:

``` haskell
type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

type WarnMsg = ErrMsg
data ErrMsg = ErrMsg {
    errMsgSpan        :: SrcSpan,
    errMsgContext     :: PrintUnqualified,
    errMsgDoc         :: ErrDoc,
    -- | This has the same text as errDocImportant . errMsgDoc.
    errMsgShortString :: String,
    errMsgSeverity    :: Severity,
    errMsgReason      :: WarnReason
    }

data ErrDoc = ErrDoc {
    -- | Primary error msg.
    errDocImportant     :: [MsgDoc],
    -- | Context e.g. \"In the second argument of ...\".
    errDocContext       :: [MsgDoc],
    -- | Supplementary information, e.g. \"Relevant bindings include ...\".
    errDocSupplementary :: [MsgDoc]
    }

type WarnMsg = ErrMsg

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive

  | SevDump
    -- ^ Log message intended for compiler developers
    -- No file/line/column stuff

  | SevInfo
    -- ^ Log messages intended for end users.
    -- No file/line/column stuff.

  | SevWarning
  | SevError
    -- ^ SevWarning and SevError are used for warnings and errors
    --   o The message has a file/line/column heading,
    --     plus "warning:" or "error:",
    --     added by mkLocMessags
    --   o Output is intended for end users

data WarnReason
  = NoReason
  -- | Warning was enabled with the flag
  | Reason !WarningFlag
  -- | Warning was made an error because of -Werror or -Werror=WarningFlag
  | ErrReason !(Maybe WarningFlag)
```

</details>

# Steps

The first part of the plan consists in making core pieces of the error infrastructure a little bit more permissive/generic. We are manipulating `Messages` values in the parser and the `TcRn` monads and are planning to use dedicated (subsystem-specific) error types, so we want to parametrize `Messages` with warning and error types. We might want to first introduce a type for error types and only later introduce a parameter for warnings.

``` haskell
type Messages e = (WarningMessages, ErrorMessages e)
type WarningMessages = Bag WarnMsg
type ErrorMessages = Bag (ErrMsg e)

data ErrMsg e = ErrMsg {
    errMsgSpan        :: SrcSpan,
    errMsgContext     :: PrintUnqualified,
    errMsgPayload     :: e,
    errMsgShortString :: String, -- maybe this could be removed?
    errMsgSeverity    :: Severity,
    errMsgReason      :: WarnReason
    }
```

It would be handy to make polymorphic all the `ErrMsg` or `Messages` manipulation functions that can be, as a way to identify bulks of functions which _do_ need to know about the error type, or at least know how to do something particular with it (e.g rendering to `ErrDoc`/`SDoc`). The other functions will become usable in all subsystems error types for free, further down the road, once those are introduced. For this first step, to quickly go back to a state where GHC builds again, we could simply address all the resulting breakage by instantiating `e` to `ErrDoc` in all use sites for the error infrastructure.

We could then get subsystems to define their own error types. For now, we would want them to be mere wrappers around `ErrDoc`, that the `e` type parameter generalizes in the aforementioned code.

``` haskell
-- somewhere in the parser
data PsError = PsErrorDoc ErrDoc

-- in GHC.Tc.Monad/Types?
data TcRnError = TcRnErrorDoc ErrDoc

-- somewhere under GHC.Driver
data GhcError
  = PsErr PsError
  | TcRnErr TcRnError
  | Other ErrDoc -- an escape hatch, if necessary? 
```

With those types in place, we could begin instantiating `e` to the relevant type for all use sites of the error infrastructure. The parser could would deal with `Messages PsError` values, the renamer and typechecker would produce `Message TcRnError` values, and so on.

Finally, we could start turning concrete errors into dedicated constructors of `PsError`/`TcRnError`. Starting slowly with simple [`not in scope` errors](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Rename/Unbound.hs#L64) and the likes, before converting over [the entire typechecking error infrastructure](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Errors.hs) and more.

This might involve systematically retaining a bit more information (context lines for the typechecker, for instance) and therefore might give rise to some more generic error infrastructure constructs. This page will be updated to incorporate such details once they are figured out.

At the "top level", in the driver, where we call the different subsystems to process Haskell modules, we would end up accumulating and reporting `GhcError` values. The goal is to have the GHC program emit the exact same error as it does today, but affect the API in such a way that GHC API users would at this point get a chance to work with the algebraic error descriptions, making inspection and custom treatment a whole lot easier to implement. We could perhaps even demonstrate it at this point by implementing a little "demo" of sorts for this new way to consume errors.

# Warnings



Later, we plan to get rid of warnings as a separate data type; instead they'll just be an `ErrMsg` with a `Severity` that says it's a warning.   We can promote a warning to become an error, if necessary, by changing its severity.

But meanwhile, `WarnMsg` will be a synonym for `ErrMsg ErrDoc` this entire time. 

(Another possible later plan would be to introduce subsystem specific warning types and apply a similar design as for errors, with a toplevel `GhcWarning` sum type and `WarningMessages` becoming `Bag (WarnMsg w)` in the general case, and with `w` instantiated to the appropriate type at use sites (`PsWarning`, `TcRnWarning`, `GhcWarning`, `Outputable w => WarnMsg w`).  But that seems over-complicated compared to just getting rid of them.)

