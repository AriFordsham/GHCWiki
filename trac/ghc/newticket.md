I received the following error:
```
ghc: panic! (the 'impossible' happened)
  (GHC version 8.0.2 for x86_64-unknown-linux):
	initTc: unsolved constraints
  WC {wc_insol =
        [W] __ahZW :: t_ahZV[tau:1] (CHoleCan: _)
        [W] __ahZZ :: t_ahZY[tau:1] (CHoleCan: _)
        [W] __ai0d :: t_ai0c[tau:1] (CHoleCan: _)}
```
The following incomplete function declaration seems to be the cause of the bug:

```
fmlToLC :: Formula -> LinearConstraint
fmlToLC f = case f of 
  BoolLit{} -> error $ show $ text "fmlToLC: boolean literal" <+> pretty f
  SetLit{}  -> error $ show $ text "fmlToLC: set literal" <+> pretty f
  Unknown{} -> error $ show $ text "fmlToLC: unknown" <+> pretty f
  All{}     -> error $ show $ text "fmlToLC: universal quantifier" <+> pretty f
  Z3Lit{}   -> error $ show $ text "fmlToLC: Z3 literal" <+> pretty f
  (IntLit x) -> LCInt x
  (Var s x)  -> LCVar s x
  (Unary Neg f) -> LCLit N $ fmlToLC f 
  (Unary Not _) -> error $ show $ text "fmlToLC: boolean negation" <+> pretty f
  (Binary Plus f g) -> _ 
  (Binary Times  f g) -> _ 
  Binary{} -> error $ show $ text "fmlToLC: illegal operator" <+> pretty f
  (Ite f g h) -> _
  (Pred s x args) -> LCFun s x $ map fmlToLC args
  (Cons s x args) -> LCFun s x $ map fmlToLC args

fmlToLC BoolLit{} = error "fmlToLC: boolean literal"
fmlToLC SetLit{}  = error "fmlToLC: set literal"
fmlToLC (IntLit x) = LCInt x
fmlToLC 
```

