Currently GHC cannot be easily built with stack protector enabled mostly because using stack protector requires you to link with `libssp`.

However GHC currently does not allow you to add a global library to the link path late enough so that it can be used to satisfy ssp.
So the build system has to be patched somewhat to support this.

Note that you also need to have `libssp` installed. The easiest way to do this is to do `pacman -Sy mingw-w64-(uname -m)-gcc` to install the toolchain compiler.

Further that the following changes need to be made to various built system files (Also make sure you have `/mingw64/bin` on your `PATH`.

```
diff --git a/aclocal.m4 b/aclocal.m4
index 78ce4ea9f3..dc05d0e339 100644
--- a/aclocal.m4
+++ b/aclocal.m4
@@ -525,8 +525,8 @@ AC_DEFUN([FP_SETTINGS],
     elif test "$EnableDistroToolchain" = "YES"
     then
         SettingsCCompilerCommand="$(basename $CC)"
-        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
-        SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2"
+        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2 -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check"
+        SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2 -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check"
         SettingsHaskellCPPCommand="$(basename $HaskellCPPCmd)"
         SettingsHaskellCPPFlags="$HaskellCPPArgs"
         SettingsLdCommand="$(basename $LdCmd) -lssp"
diff --git a/rts/package.conf.in b/rts/package.conf.in
index 8b7390865b..e8cb6b675b 100644
--- a/rts/package.conf.in
+++ b/rts/package.conf.in
@@ -47,6 +47,7 @@ extra-libraries:
                               ,"winmm"      /* for the linker */
                               ,"dbghelp"    /* for crash dump */
                               ,"psapi"      /* for process information.  */
+                              ,"ssp"      /* for process information.  */
 #endif
 #if NEED_PTHREAD_LIB
                               , "pthread"   /* for pthread_getthreadid_np, pthread_create, etc. */
diff --git a/rts/rts.cabal.in b/rts/rts.cabal.in
index 1a1eb30611..a7768f724a 100644
--- a/rts/rts.cabal.in
+++ b/rts/rts.cabal.in
@@ -97,6 +97,7 @@ library
           dbghelp
           -- for process information
           psapi
+          ssp
        -- TODO: Hadrian will use this cabal file, so drop WINVER from Hadrian's configs.
        -- Minimum supported Windows version.
        -- These numbers can be found at:
diff --git a/rules/build-prog.mk b/rules/build-prog.mk
index 0528c15fc9..9fea483910 100644
--- a/rules/build-prog.mk
+++ b/rules/build-prog.mk
@@ -265,11 +265,11 @@ $1/$2/build/tmp/$$($1_$2_PROG).dll : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$(
 else # $1_$2_PROG_NEEDS_C_WRAPPER=NO
 ifeq "$$($1_$2_LINK_WITH_GCC)" "NO"
 $1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
-       $$(call cmd,$1_$2_HC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_HC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_GHC_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES))
+       $$(call cmd,$1_$2_HC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_HC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_GHC_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES)) -lssp

 else
 $1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
-       $$(call cmd,$1_$2_CC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_CC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_EXTRA_CC_OPTS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES))
+       $$(call cmd,$1_$2_CC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_CC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_EXTRA_CC_OPTS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES)) -lssp
 endif
 endif # $1_$2_PROG_NEEDS_C_WRAPPER
diff --git a/utils/ghc-cabal/ghc.mk b/utils/ghc-cabal/ghc.mk
index a964e55070..6a5fd13d20 100644
--- a/utils/ghc-cabal/ghc.mk
+++ b/utils/ghc-cabal/ghc.mk
@@ -80,7 +80,7 @@ $(ghc-cabal_DIST_BINARY): $(CABAL_LEXER_DEP) utils/ghc-cabal/Main.hs $(TOUCH_DEP
               -Ilibraries/text/include \
               -ilibraries/parsec/src \
               $(utils/ghc-cabal_dist_EXTRA_HC_OPTS) \
-              $(EXTRA_HC_OPTS)
+              $(EXTRA_HC_OPTS) -lssp
        "$(TOUCH_CMD)" $@
 endif
diff --git a/text.cabal b/text.cabal
index 6b46fd6..20c1bcf 100644
--- a/text.cabal
+++ b/text.cabal
@@ -186,6 +186,8 @@ library
       cpp-options: -DINTEGER_GMP
       build-depends: integer-gmp >= 0.2 && < 1.1

+  extra-libraries: ssp
+
   -- compiler specification
   default-language: Haskell2010
   default-extensions:
```

Note that the build system sometimes seems to drop some of these changes. When done check that your `config.mk` contains

```
SettingsCCompilerCommand = $$tooldir/mingw/bin/gcc.exe
SettingsHaskellCPPCommand = $$tooldir/mingw/bin/gcc.exe
SettingsHaskellCPPFlags = -E -undef -traditional
SettingsCCompilerFlags = -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check -lssp
SettingsCxxCompilerFlags = -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check -lssp
SettingsCCompilerLinkFlags = -lssp 
SettingsCCompilerSupportsNoPie = NO
SettingsLdCommand = $$tooldir/mingw/bin/ld.exe
SettingsLdFlags = -lssp
SettingsMergeObjectsCommand = $$tooldir/mingw/bin/ld.exe
SettingsMergeObjectsFlags = -r
SettingsArCommand = $$tooldir/mingw/bin/ar.exe
SettingsRanlibCommand = $$tooldir/mingw/bin/ranlib.exe
SettingsDllWrapCommand = $$tooldir/mingw/bin/dllwrap.exe
SettingsWindresCommand = $$tooldir/mingw/bin/windres.exe
SettingsLibtoolCommand = C:/tools/msys64/usr/bin/libtool
SettingsTouchCommand = $$topdir/bin/touchy.exe
SettingsClangCommand = clang
SettingsLlcCommand = llc
SettingsOptCommand = opt
```

along with 

```
CONF_CC_OPTS_STAGE0 = -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check -lssp
CONF_CC_OPTS_STAGE1 = -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check -lssp
CONF_CC_OPTS_STAGE2 = -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check -lssp
CONF_GCC_LINKER_OPTS_STAGE0 = -lssp
CONF_GCC_LINKER_OPTS_STAGE1 = -lssp
CONF_GCC_LINKER_OPTS_STAGE2 = -lssp
CONF_LD_LINKER_OPTS_STAGE0 = -lssp
CONF_LD_LINKER_OPTS_STAGE1 = -lssp
CONF_LD_LINKER_OPTS_STAGE2 = -lssp
CONF_CPP_OPTS_STAGE0 = 
CONF_CPP_OPTS_STAGE1 = 
CONF_CPP_OPTS_STAGE2 = 
CONF_HC_OPTS_STAGE0 = -lssp
CONF_HC_OPTS_STAGE1 = -lssp
CONF_HC_OPTS_STAGE2 = -lssp
```

I found it easier to just modify your `config.mk` after running configure.

When configuring you can use

```
CFLAGS="-fno-omit-frame-pointer -fsanitize-address-use-after-scope -fsanitize-undefined-trap-on-error -fstack-protector-all -fstack-check"
```

to enable both stack protected and sanitizer support.  Note that because mingw-w64 does not build `libsanitizer` on Windows you must tell the compiler that you instead want to trap with an undefined insn error.  This mode does not require `libsantizer` support.