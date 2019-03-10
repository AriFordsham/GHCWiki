# Emacs tips and tricks


Most of us use Emacs for GHC development. This page lists various Emacs configuration tips and tricks used by GHC developers. It is meant to be self-contained so that you can grab relevant piece of configuration, put it into your config file and benefit from it instantly. The only exception are two larger extensions for Emacs (haskell-mode and ghc-mod), where we direct you to external installation instructions and tutorials.


Each entry includes a short description and instruction how to enable given setting (if it contains only a block of lisp code it means you need to add it to your `.emacs` configuration file). All configuration is given for Emacs 24, unless otherwise noted. 


Most of the packages used below are bundled with Emacs. If a package is not part of your Emacs installation you need to install it by yourself. You can do it manually by downloading relevant `*.el` file and putting it in your configuration directory or you can use ELPA -  Emacs package management system. A five minute introduction to ELPA can be found [ here](http://ergoemacs.org/emacs/emacs_package_system.html).

# General

## IDO mode

**Description**: IDO stands for Interactively Do Things and it greatly improves file opening and switching between buffers. When opening a file it shows list of files and directories in current directory, allows to navigate the directory tree in an easy manner, provides intuitive filtering capabilities and allows to select a file by selecting its name using arrow keys. Similar behaviour is provided when switching between opened buffers. A nice introductory tutorial to IDO can be found [ here](http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/).

**How to enable**:

```
(setqido-enable-flex-matchingt)(setqido-everywheret)(ido-mode1)
```

## Make the quotes in GHC error messages display nicely


If you run a shell within emacs, you'll see weird escape sequences when GHC displays error message involving Unicode forward or back quotes, eg

```wiki
    The type signature for \342\200\233foo\342\200\231 lacks an accompanying binding
      (The type signature must be given where \342\200\233foo\342\200\231 is declared)
```


To make emacs display this Unicode nicely, use this (see [\#2507](https://gitlab.haskell.org//ghc/ghc/issues/2507))

```wiki
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
```

## Disable tabs for indentation

**Description**: We used to use tabs for indentation, but now we aim to have no tabs in the source code. There is a hook that will prevent you from pushing tabs into repository (unless file already contained tabs). This setting will prevent you from introducing tabs in the source code.

**How to enable**:

```
(setq-defaultindent-tabs-modenil)
```

## Highlight text beyond 80th column

**Description**: If you have a tendency to write too long lines of code this will help you by highlighting text beyond 80th column.

**How to enable**:

```
(require'whitespace)(setqwhitespace-style'(facelines-tail))(setqwhitespace-line-column80)(global-whitespace-modet)
```

## Automatically removes trailing whitespaces when file is saved

**Description**: Currently source code of GHC contains lots of trailing whitespaces, which means that **this setting is dangerous**. It will remove ALL trailing whitespaces in every file that you edit, so you might have one or two lines changed by you and a hundred lines automatically changed by removing trailing whitespaces. This will require you to separate whitespaces into a separate commit by using `git add -i`. This is tedious, so be warned.

**How to enable**:

```
(add-hook'before-save-hook'delete-trailing-whitespace)
```

## Highlight trailing whitespaces

**Description**: Automatic removal of trailing whitespaces described above can be a bit inconvenient to use. One of the alternative approaches to problem of trailing whitespaces is making them visible, so that you notice when you accidentally introduce them into a file. You will also see already existing trailing whitespaces. For more details see [ Useless Whitespace](http://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html) section of Emacs documentation.

**How to enable**:

```
(setq-defaultshow-trailing-whitespacet)(setq-defaultindicate-empty-linest)
```

## Kill whole line and newline with C-k if at beginning of line

**Description**: IF you place cursor at the beginning of line, the default behaviour of C-k is to fill all text until newline, but not the newline itself. You need to type C-k again to remove that newline. This setting allows to avoid typing `C-k` twice by killing all text until the newline and newline itself (but only when cursor is placed at the beginning of a line).

**How to enable**:

```
(setqkill-whole-linet)
```

## Toggle line number display

**Description**: Allows you to toggle line number display with F11. For those who like to see line numbers at the beginning of each line.

**How to enable**:

```
(global-set-key(kbd"<f11>")'global-linum-mode)
```

## Jump to first error

**Description**: Allows you to jump to the first error in compiled or interpreted code.

**How to enable**:

```
(global-set-key(kbd"M-g M-f")'first-error)
```


On my setup, this won't work the first time a file is loaded into GHCi. I just try to load the file a second time, and then I'm set for the rest of the session.

## Always display column number in mode line

**Description**: By default Emacs only displays line number in the mode line. This setting adds column number.

**How to enable**:

```
(column-number-mode1)
```

## Switch between windows with Alt-\<window number\>

**Description**: Enhances switching between multiple windows. To switch to another window use pressing Alt-\<window number\>. Number of each window is displayed in red in the mode line.

**How to enable**:
You need to install [ window-number](http://www.emacswiki.org/emacs/window-number.el) extension and add this to your configuration:

```
(require'window-number)(window-number-mode)(window-number-meta-mode)
```

## Switch to next/previous buffer with a single key

**Description**: This extension allows you to switch between next and previous buffer with F9 and F10 (by default). Emacs buffers (like \*messages\*) are ignored - use Shift-F9 and Shift-F10 to include them as well. Note that this extension maintains a list of buffers. After selecting a buffer it is moved to the beginning of the list. This means that buffers are ordered by the time of their recent use.

**How to enable**:
You need to install [ cycle-buffer](http://www.emacswiki.org/emacs/cycle-buffer.el) extension and add this to your configuration:

```
(autoload'cycle-buffer"cycle-buffer""Cycle forward."t)(autoload'cycle-buffer-backward"cycle-buffer""Cycle backward."t)(autoload'cycle-buffer-permissive"cycle-buffer""Cycle forward allowing *buffers*."t)(autoload'cycle-buffer-backward-permissive"cycle-buffer""Cycle backward allowing *buffers*."t)(autoload'cycle-buffer-toggle-interesting"cycle-buffer""Toggle if this buffer will be considered."t)(global-set-key[(f9)]'cycle-buffer-backward)(global-set-key[(f10)]'cycle-buffer)(global-set-key[(shiftf9)]'cycle-buffer-backward-permissive)(global-set-key[(shiftf10)]'cycle-buffer-permissive)
```

## Untabifying a buffer

**Description**: Slowly, GHC is moving away from tabs. After modifying a file and committing the changes, we tend to detab the file completely, and set `-fwarn-tabs` to make sure they don't slip in again. Normally it's convenient to immediately detab the file and commit that afterwords, while the buffer is open

**How to enable**:
Add this to your configuration, and afterwords you can run `M-x untabify-buffer` to nuke all the tabs:

```
(defununtabify-buffer()"Untabify current buffer."(interactive)(save-excursion(untabify(point-min)(point-max))))
```

# Haskell-specific

## Haskell mode

**Description**: Haskell mode is a major mode for Emacs. Features include: syntax highlighting, GHCi integration and automatic indentation. See [ online manual](http://haskell.github.io/haskell-mode/manual/latest/) for details.

**How to enable**: See installation instructions on [ github](https://github.com/haskell/haskell-mode).

## ghc-mod

**Description**: Extends haskell-mode. Features include: building on the fly with flymake, hlint integration, auto-completion of keywords. See [ here](http://www.mew.org/~kazu/proj/ghc-mod/en/emacs.html) for a complete list.

**How to enable**: See installation instructions [ here](http://www.mew.org/~kazu/proj/ghc-mod/en/).

## Using tags to quickly locate definitions in a project

**Description**: Emacs can use a special index file, the `TAGS` file, that stores locations of various definitions (functions, modules, data types) in a given directory. Once you've generated `TAGS` file (see installation instructions below) you can type `M-.` and enter name of identifier definition to jump to. Emacs by default jumps to identifier currently under the cursor.

**How to enable**: Begin by installing `hasktags` package from Hackage:

```wiki
cabal install hasktags
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
(setqghc-location"~/ghc");; change as necessary
```


If you switch between active trees, you must use a `setq` to change this variable, with, say, M-: `(setq ghc-location "~/other-ghc")` \<Enter\>.

## Searching the GHC source tree with a hotkey

**Description**: Even with hasktags, sometimes you need to search within the source tree. `rgrep` does this job well, but it asks too many redundant questions. So, the following code optimizes for a search just within the `compiler` directory:

**How to enable**: The following code binds this search to M-c, but you may want your own key combination. Note that it uses `ghc-location`, set above.

```
;; search withing GHC compiler code(defunrgrep-ghc(regexp)(interactive(list(progn(grep-compute-defaults)(grep-read-regexp))))(rgrepregexp"*hs"(concatghc-location"/compiler/")))(global-set-key(kbd"M-c")'rgrep-ghc)
```

## Building GHC with a hotkey

**Description**: By having GHC be built with a hotkey, working on GHC becomes much more interactive. In a typical session, though, I have to change the actual compilation command based on my needs. So, by default, "compiling GHC" means fast-building the stage-2 compiler, but I do frequently change `ghc-compile`.

**How to enable**:

```
(defuncompile-ghc()(interactive)(let((compile-command(if(boundp'ghc-compile)(concat"cd "ghc-location"; "ghc-compile)(concat"cd "ghc-location"/ghc; make 2"))))(compilecompile-command))(set-buffer"*compilation*")(setqdefault-directoryghc-location))(defunset-compile-ghc()(local-set-key(kbd"C-q")'compile-ghc))(add-hook'haskell-mode-hook'set-compile-ghc)
```


A few things to note here:

- The above code binds C-q (perhaps a bad combination, as I've accidentally quit Emacs from my Mac with the wrong modifier key!) to compiling GHC from *any* Haskell file, even those unrelated to GHC. But, when I'm working outside of GHC, I tend to use C-c C-l to load into GHCi, so this works out OK.
- By default, as said above, this will compile the stage 2 compiler for the GHC at `ghc-location`. If you set `ghc-compile` with, say, M-: `(setq ghc-compile "cd compiler; make 1")` \<Enter\>, then this will build the stage 1 compiler.
