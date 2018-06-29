# Refactoring GHC Types


This page is to document several ideas for how to refactor the way types are stored and processed within GHC. The general goal here is to clean up code, but it is also possible we'll be able to improve error messages through this work.


This page was started in June 2018.

## Desugaring types separately


Currently (June 2018), GHC sports these two functions:

```
tc_hs_type::TcTyMode->HsTypeGhcRn->TcKind->TcMTcType-- in TcHsType.hstcExpr::HsExprGhcRn->ExpRhoType->TcM(HsExprGhcTcId)-- in TcExpr.hs
```