[Spacemacs](https://www.spacemacs.org/) is an Emacs (configuration) distribution. It mainly consists of pre-configured sets of packages that are organized in layers (e.g. there is a `haskell` layer). With Spacemacs you can relatively quickly get an "IDE experience" for GHC development.

Topics regarding Emacs configuration in general can be found here: [Emacs](emacs)

If you want to see, what you may get, there is a Docker-based showcase with a Spacemacs environment that is fully configured for GHC development: [github.com/supersven/ghc-spacemacs-docker](https://github.com/supersven/ghc-spacemacs-docker)

# Table of Contents
[[_TOC_]]

# Prerequisites
## ghc.nix
This page assumes that you are using [`nix`](https://nixos.org/) and [`ghc.nix`](https://github.com/alpmestan/ghc.nix).

The installation of Nix depends on your system. Please see https://nixos.org/nix/manual/#ch-installing-binary .

`ghc.nix` is "installed" by cloning it into your GHC source folder, e.g.

```
cd /home/sven/src/ghc
git clone https://github.com/alpmestan/ghc.nix.git
```

## Spacemacs on `develop` branch
Support for the `lsp` backend in the `haskell`-layer is currently only available on the `develop`-branch.
https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/haskell#lsp

To get it, you need to check it out. The Spacemacs code usually resides in `~/.emacs.d`.

```bash
cd ~/.emacs.d
git checkout --track origin/develop
git pull
``` 

If Spacemacs is already running, restart it and update all packages.

# Haskell

## ghcide

[`ghcide`](https://github.com/digital-asset/ghcide) implements the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/). It is a tool that provides IDE features like type checking, symbol navigation and information on hover.

In simple words: Emacs doesn't understand Haskell, ghcide does. :smile:  

### How to enable
#### Get `ghcide` via `ghc.nix`
To use `ghcide` you have to make sure that it's in your environment. `ghc.nix` provides a parameter - `withIde` - for this.

Later we'll see that we need it in a `nix-shell` environment. So, add a `shell.nix` file with `withIde = true`. 

`./shell.nix`:
```nix
import ./ghc.nix/default.nix {
  bootghc = "ghc865";
  withIde = true;
  withHadrianDeps = true;
  cores = 8;
  withDocs = false;
}
```

The other parameters are optional and only provided as examples that you can configure much more with `ghc.nix`.

##### Cachix
You can save a lot of compilation time by using a pre-built ("cached") `ghcide`.

To enable the `cachix` cache for `ghcide`:

```shell
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use ghcide-nix
```

Of course, you only need the first line if `cachix` isn't already installed.

#### Configure Spacemacs to use `ghcide` with `nix-shell`

Configure two layers, `lsp` and `haskell`, to use `ghcide` in a `nix-shell` environment:

```elisp
...

;; List of configuration layers to load.
dotspacemacs-configuration-layers
'(
  (lsp :variables
       default-nix-wrapper (lambda (args)
                             (append
                              (append (list "nix-shell" "-I" "." "--pure" "--command" )
                                      (list (mapconcat 'identity args " "))
                                      )
                              (list (nix-current-sandbox))
                              )
                             )

       lsp-haskell-process-wrapper-function default-nix-wrapper
       lsp-haskell-process-path-hie "ghcide"
       lsp-haskell-process-args-hie '()
       )

  (haskell :variables
           haskell-enable-hindent t
           haskell-completion-backend 'lsp
           haskell-process-type 'cabal-new-repl
           )

...
```

And load the `nix-sandbox` package on statup:

```elisp
...

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(nix-sandbox)

...
```

#### `compiler/` and `hadrian/` are two distinct projects

Unfortunately [`projectile`](https://projectile.readthedocs.io/en/latest/) recognizes GHC and hadrian as **one** project.

To make calls to `ghcide` with different parameters, the distinction between GHC and hadrian is important.

Add an empty `hadrian/.projectile` file:
```
touch hadrian/.projectile
```

#### Configure different `ghcide` command line arguments

To test if `ghcide` works, you can call it directly.

For GHC:
```bash
nix-shell --command "ghcide compiler"
```

You should see a lot of output and finally a success message:
```
...

Files that worked: 469
Files that failed: 0
Done
```

For hadrian:
```bash
nix-shell --command "ghcide --cwd hadrian ."
```

```
Files that worked: 96
Files that failed: 0
Done
```

To configure different `ghcide` parameters per source folder, we can use `.dir-locals.el` files.

`.dir-locals.el`:
```elisp
((nil
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (buffer-file-coding-system . utf-8-unix))

 (haskell-mode
   (lsp-haskell-process-args-hie .
                                 ("--cwd /home/sven/src/ghc compiler")))
 )
```
(Replace `/home/sven/src/ghc` with the path to your GHC source directory.)

`hadrian/.dir-locals.el`:
```elisp
 ((haskell-mode
  (lsp-haskell-process-args-hie .
                                ("--cwd /home/sven/src/ghc/hadrian ."))))
```
(Replace `/home/sven/src/ghc` with the path to your GHC source directory.)

`--cwd` (*Current Working Directory*) makes sure that `ghcide` runs on the root of the project and not in the directory of the file.

The settings for [`indent-tabs-mode`](https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html), [`fill-column`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fill-Commands.html) and [`buffer-file-coding-system`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Coding.html) are those preferred by the GHC project. "dir-local" variables are inherited from parent directories to their childs.

#### Starting

Please make sure that you open first a Haskell file under `hadrian/` and then a file under `compiler/`. Otherwise Spacemacs will automatically assume, that files under `hadrian/` belong to the same workspace as `compiler/`.

### Troubleshooting

This setup is pretty complicated. To find errors, you can check several layers.

I would propose this order:
1. **Nix** - *Can I correctly instantiate the nix-environment?* *Does it contain `ghcide`?*
1. **ghcide** - *Can `ghcide` be run on the command line?*
1. **lsp-mode (Emacs)** - *Are there any error messages in the `lsp` buffers?*

#### Nix
```bash
nix-shell --pure shell.nix --command "which ghcide"
```

#### ghcide
```
nix-shell --pure shell.nix --command "ghcide compiler"
nix-shell --pure shell.nix --command "ghcide --cwd hadrian ."
```

#### lsp-mode (Emacs)

##### Enable message tracing
`M-x customize-mode` :arrow_right_hook: `lsp-mode`
Menu entry: *Lsp Server Trace*

##### Increase response timeout
`M-x customize-mode` :arrow_right_hook: `lsp-mode`
Menu entry: *Lsp Response Timeout*

##### Buffers

###### \*lsp-log\*
Shows how `ghcide` is called.

For example:
```
Command "nix-shell -I . --pure --command ghcide --lsp --cwd /home/sven/src/ghc compiler /home/sven/src/ghc/shell.nix" is present on the path.
Found the following clients for /home/sven/src/ghc/compiler/simplCore/CoreMonad.hs: (server-id hie, priority 0)
The following clients were selected based on priority: (server-id hie, priority 0)
Command "nix-shell -I . --pure --command ghcide --lsp --cwd /home/sven/src/ghc/hadrian . /home/sven/src/ghc/hadrian/shell.nix" is present on the path.
Found the following clients for /home/sven/src/ghc/hadrian/UserSettings.hs: (server-id hie, priority 0)
The following clients were selected based on priority: (server-id hie, priority 0)
Buffer switched - ignoring response. Method textDocument/hover
```

###### *\*lsp-log\*: hie: [SESSION_NUMBER]

If you've enabled message tracing (see above), these buffers contain all requests and responses of the *Language Server Protocol* regarding one session.

# C

There are three LSP backends for C to choose from: `clangd` (default in Spacemacs), `ccls` and `cquery`.

The `cquery` project seems to be abandoned. 

Both, `clangd` and `ccls` (can) use a [`compile_commands.json`](https://clang.llvm.org/docs/JSONCompilationDatabase.html) (*JSON Compilation Database*) file as configuration.

Because I (@supersven) got the best results with `ccls` (it was able to handle header files better), we'll continue with it. But configuring `clangd` should be very simple, too.

## Install `ccls`

```shell
nix-env -i ccls
```

## Generate compile_commands.json

```shell
nix-shell -p bear --command 'bear hadrian/build.sh -j12 --freeze1 --flavour=Devel2 stage2:lib:rts'
```

[`bear`](https://github.com/rizsotto/Bear) intercepts all calls to the C compiler. This way it can write a `compile_commands.json` that contains all compilation arguments and flags needed for each C file.

## Configure `c-c++` layer

In `.spacemacs`:
```elisp
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
...
     (c-c++ :variables c-c++-backend 'lsp-ccls)
...
     )

```

For more details about LSP backend configuration, please see: https://develop.spacemacs.org/layers/+lang/c-c++/README.html#backends


# Historical
## Dante
:warning: `ghcide` support is pretty good now and the project is gaining momentum. If you aren't sure that you want to use `dante`, you probably want to use `ghcide` (at least for GHC development).

The author of this section (@supersven) switched to `ghcide`, so it might be outdated.

---

**Description**: This section is a bit special because it applies to a very specific setup: Using Spacemacs (an Emacs configuration distribution) with `dante-mode` as editor and `nix-shell` for building GHC.

The initial setup is a bit cumbersome, but you'll gain syntax highlighting, type checking / info and navigation ("Jump to Definition").

Dante is currently only available on the `develop` branch of Spacemacs.
```bash
cd ~/.emacs.d
git checkout develop
```

Create a file `.dir-locals.el` in the root folder of the GHC project (e.g. `~/src/ghc/.dir-locals.el` on my machine):
```elisp
((haskell-mode
  (dante-repl-command-line . ("nix-shell" "--arg" "cores" "8" "--arg" "version" "8.9" "--arg" "withHadrianDeps" "true" "--arg" "bootghc" "\"ghc864\"" "--pure" "ghc.nix" "--run" "hadrian/ghci.sh"))))
```
As you easily recognize, `dante-repl-command-line` is set to running `hadrian/ghci.sh` in a `nix-shell` environment. The `--arg`s are how I use `ghc.nix`, of course you can and should adjust them to your needs.

If you now open a Haskell file in the GHC project, `dante-mode` should automatically start and use `nix-shell` to call `hadrian/ghci.sh`.

**Troubleshooting**
- Configure `dante-mode` to print debug information in a separate buffer: `Meta+x customize-group` `dante`
- Try to run `hadrian/ghci.sh` with `nix-shell` manually to see if this works. I.e. run `nix-shell [args omitted] ghc.nix --run hadrian/ghci.sh` in your shell.

**ToDo**: Some features of `dante-mode` don't seem to work. Maybe using `utils/ghc-in-ghci/run.sh` would lead to better results, but I haven't tested this, yet.