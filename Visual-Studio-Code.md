## Haskell

If all dependencies to build GHC (with Hadrian) are installed, the [Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) works out of the box. It installs the required [Haskell Language Server](https://github.com/haskell/haskell-language-server) automatically.

## C

I (Sven) got the best results with the [clangd plugin](https://marketplace.visualstudio.com/items?itemName=llvm-vs-code-extensions.vscode-clangd). It requires `clangd` to be installed and a `compile_commands.json` file to be in place.

`compile_commands.json` contains all build parameters for all C files. It can easily be generated with [bear](https://github.com/rizsotto/Bear). You just have to prefix your build command with `bear`, e.g.

```
bear make [...]
```

```
bear hadrian/build [...]
```

## Nix

If you're using [ghc.nix](https://github.com/alpmestan/ghc.nix), you'll very likely want use it the provide the dependencies for the previously described plugins.

This can easily be done with the [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector) plugin.