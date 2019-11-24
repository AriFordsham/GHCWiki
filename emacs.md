# Emacs tips and tricks


Most of us use Emacs for GHC development. This page lists various Emacs configuration tips and tricks used by GHC developers. It is meant to be self-contained so that you can grab relevant piece of configuration, put it into your config file and benefit from it instantly. The only exception are two larger extensions for Emacs (haskell-mode and ghc-mod), where we direct you to external installation instructions and tutorials.


Each entry includes a short description and instruction how to enable given setting (if it contains only a block of lisp code it means you need to add it to your `.emacs` configuration file). All configuration is given for Emacs 24, unless otherwise noted. 



Most of the packages used below are bundled with Emacs. If a package is not part of your Emacs installation you need to install it by yourself. You can do it manually by downloading relevant `*.el` file and putting it in your configuration directory or you can use ELPA -  Emacs package management system. A five minute introduction to ELPA can be found [here](http://ergoemacs.org/emacs/emacs_package_system.html).


# General


## IDO mode



**Description**: IDO stands for Interactively Do Things and it greatly improves file opening and switching between buffers. When opening a file it shows list of files and directories in current directory, allows to navigate the directory tree in an easy manner, provides intuitive filtering capabilities and allows to select a file by selecting its name using arrow keys. Similar behaviour is provided when switching between opened buffers. A nice introductory tutorial to IDO can be found [here](http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/).



**How to enable**:


```
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
```

## Disable tabs for indentation



**Description**: We used to use tabs for indentation, but now we aim to have no tabs in the source code. There is a hook that will prevent you from pushing tabs into repository (unless file already contained tabs). This setting will prevent you from introducing tabs in the source code.



**How to enable**:


```
(setq-default indent-tabs-mode nil)
```

## Highlight text beyond 80th column



**Description**: If you have a tendency to write too long lines of code this will help you by highlighting text beyond 80th column.



**How to enable**:


```
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)
```

## Automatically removes trailing whitespaces when file is saved



**Description**: Currently source code of GHC contains lots of trailing whitespaces, which means that **this setting is dangerous**. It will remove ALL trailing whitespaces in every file that you edit, so you might have one or two lines changed by you and a hundred lines automatically changed by removing trailing whitespaces. This will require you to separate whitespaces into a separate commit by using `git add -i`. This is tedious, so be warned.



**How to enable**:


```
(add-hook 'before-save-hook 'delete-trailing-whitespace)
```

## Highlight trailing whitespaces



**Description**: Automatic removal of trailing whitespaces described above can be a bit inconvenient to use. One of the alternative approaches to problem of trailing whitespaces is making them visible, so that you notice when you accidentally introduce them into a file. You will also see already existing trailing whitespaces. For more details see [Useless Whitespace](http://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html) section of Emacs documentation.



**How to enable**:


```
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
```

## Kill whole line and newline with C-k if at beginning of line



**Description**: IF you place cursor at the beginning of line, the default behaviour of C-k is to kill all text until newline, but not the newline itself. You need to type C-k again to remove that newline. This setting allows to avoid typing `C-k` twice by killing all text until the newline and newline itself (but only when cursor is placed at the beginning of a line).



**How to enable**:


```
(setq kill-whole-line t)
```

## Toggle line number display



**Description**: Allows you to toggle line number display with F11. For those who like to see line numbers at the beginning of each line.



**How to enable**:


```
(global-set-key (kbd "<f11>") 'global-linum-mode)
```

## Jump to first error



**Description**: Allows you to jump to the first error in compiled or interpreted code.



**How to enable**:


```
(global-set-key (kbd "M-g M-f") 'first-error)
```


On my setup, this won't work the first time a file is loaded into GHCi. I just try to load the file a second time, and then I'm set for the rest of the session.


## Always display column number in mode line



**Description**: By default Emacs only displays line number in the mode line. This setting adds column number.



**How to enable**:


```
(column-number-mode 1)
```

## Switch between windows with Alt-\<window number\>

**Description**: Enhances switching between multiple windows. To switch to another window use pressing Alt-\<window number\>. Number of each window is displayed in red in the mode line.

**How to enable**:
You need to install [window-number](http://www.emacswiki.org/emacs/window-number.el) extension and add this to your configuration:


```
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)
```

## Switch to next/previous buffer with a single key

**Description**: This extension allows you to switch between next and previous buffer with F9 and F10 (by default). Emacs buffers (like \*messages\*) are ignored - use Shift-F9 and Shift-F10 to include them as well. Note that this extension maintains a list of buffers. After selecting a buffer it is moved to the beginning of the list. This means that buffers are ordered by the time of their recent use.

**How to enable**:
You need to install [cycle-buffer](http://www.emacswiki.org/emacs/cycle-buffer.el) extension and add this to your configuration:


```
(autoload 'cycle-buffer                     "cycle-buffer"
  "Cycle forward." t)
(autoload 'cycle-buffer-backward            "cycle-buffer"
  "Cycle backward." t)
(autoload 'cycle-buffer-permissive          "cycle-buffer"
  "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer"
  "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting  "cycle-buffer"
  "Toggle if this buffer will be considered." t)
(global-set-key [(f9)]        'cycle-buffer-backward)
(global-set-key [(f10)]       'cycle-buffer)
(global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
(global-set-key [(shift f10)] 'cycle-buffer-permissive)
```

## Untabifying a buffer

**Description**: Slowly, GHC is moving away from tabs. After modifying a file and committing the changes, we tend to detab the file completely, and set `-fwarn-tabs` to make sure they don't slip in again. Normally it's convenient to immediately detab the file and commit that afterwords, while the buffer is open

**How to enable**:


This is equivalent to `mark-whole-buffer` which is bound to `C-x h` by default, followed by `M-x untabify` which operates in the current region.



Add this to your configuration, and afterwords you can run `M-x untabify-buffer` to nuke all the tabs:


```
(defun untabify-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))
```

## Re-format a comment

**Description**: After writing a longer comment - either enclosed in a block `{- ... -}` or with each line preceded by `--` - you can automatically format it to a desired line width using `M-q`.

**How to enable**: Set desired line width with

```wiki
(setq-default fill-column 80)
```


When Emacs is already running you can modify this setting with `C-x f` or `M-x set-fill-column`.

# Haskell-specific

## Haskell mode

**Description**: Haskell mode is a major mode for Emacs. Features include: syntax highlighting, GHCi integration and automatic indentation. See [online manual](http://haskell.github.io/haskell-mode/manual/latest/) for details.

**How to enable**: See installation instructions on [github](https://github.com/haskell/haskell-mode).

## ghc-mod

:warning: This section is very likely outdated. See https://github.com/DanielG/ghc-mod#legacy for details.

**Description**: Extends haskell-mode. Features include: building on the fly with flymake, hlint integration, auto-completion of keywords. See [here](http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html) for a complete list.

**How to enable**: See installation instructions [here](http://www.mew.org/~kazu/proj/ghc-mod/en/).

## Using tags to quickly locate definitions in a project

**Description**: Emacs can use a special index file, the `TAGS` file, that stores locations of various definitions (functions, modules, data types) in a given directory. Once you've generated `TAGS` file (see installation instructions below) you can type `M-.` and enter name of identifier definition to jump to. Emacs by default jumps to identifier currently under the cursor.

**How to enable**: Begin by installing `hasktags`. One option is to install the package from Hackage:

```wiki
cabal install hasktags
```

Alternatively, on Arch Linux you may want to use your package manager since `cabal install` will fail due to [missing static versions of libraries](https://wiki.archlinux.org/index.php/haskell#Problems_with_linking):

```wiki
pacman -S hasktags
```

Now go to the directory for which you want to generate tags and run:

```wiki
hasktags --ignore-close-implementation .
```


In Emacs type `M-x visit-tags-table` and point to the generated `TAGS`.

**Note on hasktags**: `hasktags` program used to generate `TAGS` file has problems with correctly recognizing declarations of value constructors in a data type. It often mistakes pattern matching of the form `(DataConstructor {})` as a data declaration and jumps to that pattern match instead of declaration.

**Note on using tags for GHC sources**: Generating tags for top directory of GHC source tree. so that index is generated for the compiler sources as well as boot libraries, gives rather bad results. The problem is that many libraries have definitions of identical functions, e.g. `integer-gmp` and `integer-simple` define the same functions and `hoopl` has lots of obsolete source files that contain definitions of exactly identical functions. This makes jumping to definitions unreliable - you will often be taken to some unused definition. Therefore we recommend to generate tags file only for `compiler/` directory.

# GHC-specific



It can be helpful to have a few commands specific to working on GHC. For these to work, Emacs must know where your local GHC tree is.


```
(setq ghc-location "~/ghc") ;; change as necessary
```


If you switch between active trees, you must use a `setq` to change this variable, with, say, M-: `(setq ghc-location "~/other-ghc")` \<Enter\>.


## Searching the GHC source tree with a hotkey



**Description**: Even with hasktags, sometimes you need to search within the source tree. `rgrep` does this job well, but it asks too many redundant questions. So, the following code optimizes for a search just within the `compiler` directory:



**How to enable**: The following code binds this search to M-c, but you may want your own key combination. Note that it uses `ghc-location`, set above.


```
  ;; search withing GHC compiler code
(defun rgrep-ghc (regexp)
  (interactive (list (progn (grep-compute-defaults) (grep-read-regexp))))
  (rgrep regexp "*hs" (concat ghc-location "/compiler/")))
(global-set-key (kbd "M-c") 'rgrep-ghc)
```

## Building GHC with a hotkey



**Description**: By having GHC be built with a hotkey, working on GHC becomes much more interactive. In a typical session, though, I have to change the actual compilation command based on my needs. So, by default, "compiling GHC" means fast-building the stage-2 compiler, but I do frequently change `ghc-compile`.



**How to enable**:


```
(defun compile-ghc ()
  (interactive)
  (let ((compile-command (if (boundp 'ghc-compile)
                             (concat "cd " ghc-location "; " ghc-compile)
                             (concat "cd " ghc-location "/ghc; make 2"))))
    (compile compile-command))
  (set-buffer "*compilation*")
  (setq default-directory ghc-location))

(defun set-compile-ghc ()
  (local-set-key (kbd "C-q") 'compile-ghc))

(add-hook 'haskell-mode-hook 'set-compile-ghc)
```


A few things to note here:

- The above code binds C-q (perhaps a bad combination, as I've accidentally quit Emacs from my Mac with the wrong modifier key!) to compiling GHC from *any* Haskell file, even those unrelated to GHC. But, when I'm working outside of GHC, I tend to use C-c C-l to load into GHCi, so this works out OK.
- By default, as said above, this will compile the stage 2 compiler for the GHC at `ghc-location`. If you set `ghc-compile` with, say, M-: `(setq ghc-compile "cd compiler; make 1")` \<Enter\>, then this will build the stage 1 compiler.

### Alternative compile command with `-ferror-spans`


```
(defun compile-ghc ()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     (if (boundp 'compilation-save-buffers-predicate) ;; since Emacs 24.1(?)
                         compilation-save-buffers-predicate))
  (let ((compile-command
         (concat "EXTRA_HC_OPTS=-ferror-spans "
                 (if (boundp 'ghc-compile)
                     (concat "cd " ghc-location "; " ghc-compile)
                   (concat "cd " ghc-location "/ghc; make 2")))))
    (compilation-start compile-command 'haskell-compilation-mode))
  (set-buffer "*haskell-compilation*")
  (setq default-directory ghc-location))
```


## Make the quotes in GHC error messages display nicely

**Description**: If you run a shell within emacs, you'll see weird escape sequences when GHC displays error message involving Unicode forward or back quotes, eg

```wiki
    The type signature for \342\200\233foo\342\200\231 lacks an accompanying binding
      (The type signature must be given where \342\200\233foo\342\200\231 is declared)
```

**How to enable**: To make emacs display this Unicode nicely, use this (see #2507)

```wiki
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
```

# Spacemacs + Dante + Nix(os)
**Description**: This section is a bit special because it applies to a very specific setup: Using Spacemacs (an Emacs configuration distribution) with `dante-mode` as editor and `nix-shell` for building GHC.

The initial setup is a bit cumbersome, but you'll gain syntax highlighting, type checking / info and navigation ("Jump to Definition").

Requirements:
- Spacemacs (http://spacemacs.org/)
- ghc.nix (https://github.com/alpmestan/ghc.nix)
- Nix(os) (https://nixos.org/)

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

# Spacemacs + ghcide + Nix(os)

## Description
How to use `ghcide` with Spacemacs and Nix(os).

### Requirements:
- Spacemacs (http://spacemacs.org/)
- ghc.nix (https://github.com/alpmestan/ghc.nix)
- Nix(os) (https://nixos.org/)

## How to enable
The rough plan is:
- Get `ghcide` via `ghc.nix`.
- Configure Spacemacs to use `ghcide` with `nix-shell` for `haskell-mode`.
- Teach Spacemacs that `compiler/` and `hadrian/` are two distinct projects.
- Configure different `ghcide` command line arguments for the two.

### Get `ghcide` via `ghc.nix`
To use `ghcide` you have to make sure that it's in your environment. `ghc.nix` provides a parameter - `withIde` - for this.

Later we'll see that we need it in a `nix-shell` environment. So, add two `shell.nix` files with `withIde = true`. 

`./shell.nix`:
```nix
import ./ghc.nix/default.nix {
  bootghc = "ghc865";
  withIde = true;
  withHadrianDeps = true;
  cores = 8;
}
```

`hadrian/shell.nix`:
```nix
import ../ghc.nix/default.nix {
  bootghc = "ghc865";
  withIde = true;
  withHadrianDeps = true;
  cores = 8;
}
```

The other parameters are optional and only provided as examples that you can configure much more with `ghc.nix` and have different configurations for hadrian and the compiler. However, I would recommend to use the same compiler version (`bootghc`); using different GHC versions on the same project sounds like asking for trouble ... :wink:

### Configure Spacemacs to use `ghcide` with `nix-shell`

Support for the `lsp` backend in the `haskell`-layer is currently only available on the `develop`-branch.
https://github.com/syl20bnr/spacemacs/tree/develop/layers/+lang/haskell#lsp

To get it, you need to check it out. The Spacemacs code usually resides in `~/.emacs.d`.

```bash
cd ~/.emacs.d
git checkout --track origin/develop
git pull
``` 

If Spacemacs is already running, restart it and update all packages.

Now you'll configure two layers, `lsp` and `haskell`, to use `ghcide` in a `nix-shell` environment:

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
   dotspacemacs-additional-packages '(direnv nix-sandbox)


...
```

The `direnv` package is optional for this tutorial, but very useful for working with Emacs and `nix-shell` environments in general. [`direnv`](https://direnv.net/) is a command line tool, that automatically loads an environment defined by a `.envrc` file, when you visit an directory that itself contains or has a parent folder that contains one.

`.envrc` can simply redirect to `nix`:
```
use_nix
```

That way the appropriate `nix` environemt is automatically loaded in the shell and - with the package `direnv` - in Emacs.
I always drop one `.envrc` into the root of my Haskell projects. Regarding GHC I've `./.envrc` and `hadrian/.envrc`.

### `compiler/` and `hadrian/` are two distinct projects

Unfortunately [`projectile`](https://projectile.readthedocs.io/en/latest/) recognizes GHC and hadrian as **one** project.

To switch to the appropriate `nix`-environment for each Haskell source file, the distinction between GHC and hadrian is important.

Adding an empty `hadrian/.projectile` file does the job.

### Configure different `ghcide` command line arguments

To test if `ghcide` works, you can call it directly.

For GHC:
```bash
nix-shell shell.nix --command "ghcide compiler"
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
cd hadrian
nix-shell shell.nix --command "ghcide ."
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

The settings for `indent-tabs-mode`, `fill-column` and `buffer-file-coding-system` are those preferred by the GHC project. "dir-local" variables are inherited from parent directories to their childs.

## Troubleshooting

This setup is pretty complicated. To find errors, you can check several layers.

I would propose this order:
1. **Nix** - *Can I correctly instantiate the nix-environment?* *Does it contain `ghcide`?*
1. **ghcide** - *Can `ghcide` be run on the command line?*
1. **lsp-mode (Emacs)** - *Are there any error messages in the `lsp` buffers?*

### Nix
```bash
nix-shell --pure shell.nix --command "which ghcide"
nix-shell --pure hadrian/shell.nix --command "which ghcide"
```

### ghcide
```
nix-shell --pure shell.nix --command "ghcide compiler"
nix-shell --pure hadrian/shell.nix --command "ghcide --cwd hadrian ."
```

### lsp-mode (Emacs)

#### Enable message tracing
`M-x customize-mode` :arrow_right_hook: `lsp-mode`
Menu entry: *Lsp Server Trace*

#### Increase response timeout
`M-x customize-mode` :arrow_right_hook: `lsp-mode`
Menu entry: *Lsp Response Timeout*

#### Buffers

##### \*lsp-log\*
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

##### *\*lsp-log\*: hie: [SESSION_NUMBER]

If you've enabled message tracing (see above), these buffers contain all requests and responses of the *Language Server Protocol* regarding one session.

## Shortcomings
Sometimes you have to be patient. "Hover" information is currently known to be slow. And creating the nix-environment for the first time might feel like you're downloading the whole internet... :wink: 