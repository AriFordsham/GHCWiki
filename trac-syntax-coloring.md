# Syntax Coloring of Source Code


Trac supports language-specific syntax highlighting of source code within wiki formatted text in [wiki processors](wiki-processors#) blocks and in the [repository browser](trac-browser). Syntax coloring is provided using [Pygments](http://pygments.org/), which covers a wide range of programming languages and other structured texts, and is actively supported. If Pygments is not available, Trac will display the content as plain text. 

### About Pygments

[Pygments](http://pygments.org/) is a highlighting library implemented in pure python, very fast, easy to extend and [ well documented](http://pygments.org/docs/).


The Pygments default style can specified in the [mime-viewer](trac-ini#) section of trac.ini. The default style can be overridden by setting a *Style* preference on the [preferences page](/trac/ghc/prefs/pygments). 

[Pygments lexer](http://pygments.org/docs/lexers/) options can be specified as [WikiProcessor](wiki-processors) arguments and defaults can be set in the [environment configuration](trac-ini#).

## Syntax Coloring Support

### Supported languages


The list of currently supported languages can be found on the [supported languages](http://pygments.org/languages/) page. The list represents the languages supported in the most recent version of Pygments, so the languages actually supported in your installation could differ if you have an older version installed. The listing of [ supported lexers](http://pygments.org/docs/lexers/) provides additional information about the default mime type to keyword mappings.


Explicit control of the mime type associated with a [WikiProcessor](wiki-processors) and file extension is available through the `mime_map` setting. For example, by default `.m` files are considered Objective-C files. In order to treat `.m` files as MATLAB files, add `text/matlab:m` to the `mime_map` setting in the [\[mimeviewer\] section of trac.ini](trac-ini#).


If a mimetype property such as `svn:mime-type` is set to `text/plain`, there is no coloring even if file is known type like `java`.

### Direct Rendering


Rich content may be directly *rendered* instead of syntax highlighted. This usually depends on which auxiliary packages are installed and on which components are activated in your setup. For example a `text/x-rst` document will be rendered via `docutils` if it is installed and the `trac.mimeview.rst.ReStructuredTextRenderer` is not disabled, and will be syntax highlighted otherwise.


In a similar way, a document with the mimetype `text/x-trac-wiki` is rendered using the Trac wiki formatter, unless the `trac.mimeview.api.WikiTextRenderer` component is disabled.



HTML documents are directly rendered only if the `render_unsafe_html` settings are enabled in the [TracIni](trac-ini) (those settings are present in multiple sections, as there are different security concerns depending where the document comes from). If you want to ensure that an HTML document gets syntax highlighted and not rendered, use the `text/xml` mimetype.


### Known MIME types



<table><tr><th>MIME Types</th>
<th><a href="/trac/ghc/wiki/WikiProcessors">WikiProcessors</a></th></tr>
<tr><th><tt>application/atom+xml</tt></th>
<th><tt>xml</tt></th></tr>
<tr><th><tt>application/json</tt></th>
<th><tt>json</tt></th></tr>
<tr><th><tt>application/json-object</tt></th>
<th><tt>json-object</tt></th></tr>
<tr><th><tt>application/kal</tt></th>
<th><tt>kal</tt></th></tr>
<tr><th><tt>application/ld+json</tt></th>
<th><tt>json-ld jsonld</tt></th></tr>
<tr><th><tt>application/msword</tt></th>
<th><tt>doc dot</tt></th></tr>
<tr><th><tt>application/pdf</tt></th>
<th><tt>pdf</tt></th></tr>
<tr><th><tt>application/postscript</tt></th>
<th><tt>postscr postscript ps</tt></th></tr>
<tr><th><tt>application/rss+xml</tt></th>
<th><tt>rss</tt></th></tr>
<tr><th><tt>application/rtf</tt></th>
<th><tt>rtf</tt></th></tr>
<tr><th><tt>application/sparql-query</tt></th>
<th><tt>sparql</tt></th></tr>
<tr><th><tt>application/vnd.wolfram.cdf</tt></th>
<th><tt>mathematica mma nb</tt></th></tr>
<tr><th><tt>application/x-awk</tt></th>
<th><tt>awk gawk mawk nawk</tt></th></tr>
<tr><th><tt>application/x-befunge</tt></th>
<th><tt>befunge</tt></th></tr>
<tr><th><tt>application/x-brainfuck</tt></th>
<th><tt>bf brainfuck</tt></th></tr>
<tr><th><tt>application/x-chaiscript</tt></th>
<th><tt>chai chaiscript</tt></th></tr>
<tr><th><tt>application/x-clojure</tt></th>
<th><tt>clj clojure</tt></th></tr>
<tr><th><tt>application/x-clojurescript</tt></th>
<th><tt>cljs clojurescript</tt></th></tr>
<tr><th><tt>application/x-coldfusion</tt></th>
<th><tt>cfm</tt></th></tr>
<tr><th><tt>application/x-csh</tt></th>
<th><tt>csh tcsh</tt></th></tr>
<tr><th><tt>application/x-cython</tt></th>
<th><tt>cython pyrex pyx</tt></th></tr>
<tr><th><tt>application/x-dos-batch</tt></th>
<th><tt>bat batch cmd dos dosbatch winbatch</tt></th></tr>
<tr><th><tt>application/x-ecl</tt></th>
<th><tt>ecl</tt></th></tr>
<tr><th><tt>application/x-elisp</tt></th>
<th><tt>elisp emacs emacs-lisp</tt></th></tr>
<tr><th><tt>application/x-evoque</tt></th>
<th><tt>evoque</tt></th></tr>
<tr><th><tt>application/x-fantom</tt></th>
<th><tt>fan</tt></th></tr>
<tr><th><tt>application/x-fish</tt></th>
<th><tt>fish fishshell</tt></th></tr>
<tr><th><tt>application/x-forth</tt></th>
<th><tt>forth</tt></th></tr>
<tr><th><tt>application/x-gooddata-maql</tt></th>
<th><tt>maql</tt></th></tr>
<tr><th><tt>application/x-httpd-lasso[89]</tt></th>
<th><tt>html+lasso</tt></th></tr>
<tr><th><tt>application/x-httpd-php5</tt></th>
<th><tt>html+php</tt></th></tr>
<tr><th><tt>application/x-hy</tt></th>
<th><tt>hylang</tt></th></tr>
<tr><th><tt>application/x-hybris</tt></th>
<th><tt>hy hybris</tt></th></tr>
<tr><th><tt>application/x-jinja</tt></th>
<th><tt>django jinja</tt></th></tr>
<tr><th><tt>application/x-jsp</tt></th>
<th><tt>jsp</tt></th></tr>
<tr><th><tt>application/x-julia</tt></th>
<th><tt>jl julia</tt></th></tr>
<tr><th><tt>application/x-kid</tt></th>
<th><tt>genshi kid xml+genshi xml+kid</tt></th></tr>
<tr><th><tt>application/x-lua</tt></th>
<th><tt>lua</tt></th></tr>
<tr><th><tt>application/x-mako</tt></th>
<th><tt>mako</tt></th></tr>
<tr><th><tt>application/x-mason</tt></th>
<th><tt>mason</tt></th></tr>
<tr><th><tt>application/x-moonscript</tt></th>
<th><tt>moon moonscript</tt></th></tr>
<tr><th><tt>application/x-myghty</tt></th>
<th><tt>myghty</tt></th></tr>
<tr><th><tt>application/x-newlisp</tt></th>
<th><tt>newlisp</tt></th></tr>
<tr><th><tt>application/x-openedge</tt></th>
<th><tt>abl openedge progress</tt></th></tr>
<tr><th><tt>application/x-perl</tt></th>
<th><tt>perl pl</tt></th></tr>
<tr><th><tt>application/x-perl6</tt></th>
<th><tt>perl6 pl6</tt></th></tr>
<tr><th><tt>application/x-pygments-tokens</tt></th>
<th><tt>raw</tt></th></tr>
<tr><th><tt>application/x-pypylog</tt></th>
<th><tt>pypy pypylog</tt></th></tr>
<tr><th><tt>application/x-python</tt></th>
<th><tt>py python sage</tt></th></tr>
<tr><th><tt>application/x-python3</tt></th>
<th><tt>py3 python3</tt></th></tr>
<tr><th><tt>application/x-qt.qbs+qml</tt></th>
<th><tt>qbs qml</tt></th></tr>
<tr><th><tt>application/x-racket</tt></th>
<th><tt>racket rkt</tt></th></tr>
<tr><th><tt>application/x-ruby</tt></th>
<th><tt>duby rb ruby</tt></th></tr>
<tr><th><tt>application/x-ruby-templating</tt></th>
<th><tt>erb</tt></th></tr>
<tr><th><tt>application/x-sas</tt></th>
<th><tt>sas</tt></th></tr>
<tr><th><tt>application/x-scheme</tt></th>
<th><tt>scheme scm</tt></th></tr>
<tr><th><tt>application/x-sh-session</tt></th>
<th><tt>console shell-session</tt></th></tr>
<tr><th><tt>application/x-shellscript</tt></th>
<th><tt>bash ksh sh shell zsh</tt></th></tr>
<tr><th><tt>application/x-shen</tt></th>
<th><tt>shen</tt></th></tr>
<tr><th><tt>application/x-smarty</tt></th>
<th><tt>smarty</tt></th></tr>
<tr><th><tt>application/x-spitfire</tt></th>
<th><tt>cheetah spitfire</tt></th></tr>
<tr><th><tt>application/x-ssp</tt></th>
<th><tt>ssp</tt></th></tr>
<tr><th><tt>application/x-standardml</tt></th>
<th><tt>sml</tt></th></tr>
<tr><th><tt>application/x-stata</tt></th>
<th><tt>do stata</tt></th></tr>
<tr><th><tt>application/x-tcl</tt></th>
<th><tt>tcl</tt></th></tr>
<tr><th><tt>application/x-terraform</tt></th>
<th><tt>terraform tf</tt></th></tr>
<tr><th><tt>application/x-thrift</tt></th>
<th><tt>thrift</tt></th></tr>
<tr><th><tt>application/x-troff</tt></th>
<th><tt>roff troff</tt></th></tr>
<tr><th><tt>application/x-turtle</tt></th>
<th><tt>turtle</tt></th></tr>
<tr><th><tt>application/x-twig</tt></th>
<th><tt>twig</tt></th></tr>
<tr><th><tt>application/x-urbiscript</tt></th>
<th><tt>urbiscript</tt></th></tr>
<tr><th><tt>application/x-yaml</tt></th>
<th><tt>yml</tt></th></tr>
<tr><th><tt>application/xhtml+xml</tt></th>
<th><tt>html</tt></th></tr>
<tr><th><tt>application/xml+evoque</tt></th>
<th><tt>xml+evoque</tt></th></tr>
<tr><th><tt>application/xml+jinja</tt></th>
<th><tt>xml+django xml+jinja</tt></th></tr>
<tr><th><tt>application/xml+lasso</tt></th>
<th><tt>xml+lasso</tt></th></tr>
<tr><th><tt>application/xml+mako</tt></th>
<th><tt>xml+mako</tt></th></tr>
<tr><th><tt>application/xml+myghty</tt></th>
<th><tt>xml+myghty</tt></th></tr>
<tr><th><tt>application/xml+php</tt></th>
<th><tt>xml+php</tt></th></tr>
<tr><th><tt>application/xml+ruby</tt></th>
<th><tt>xml+erb xml+ruby</tt></th></tr>
<tr><th><tt>application/xml+smarty</tt></th>
<th><tt>xml+smarty</tt></th></tr>
<tr><th><tt>application/xml+spitfire</tt></th>
<th><tt>xml+cheetah xml+spitfire</tt></th></tr>
<tr><th><tt>application/xml+velocity</tt></th>
<th><tt>xml+velocity</tt></th></tr>
<tr><th><tt>application/xml-dtd</tt></th>
<th><tt>dtd</tt></th></tr>
<tr><th><tt>application/xquery</tt></th>
<th><tt>xq xql xqm xquery xqy</tt></th></tr>
<tr><th><tt>application/xsl+xml</tt></th>
<th><tt>xsl</tt></th></tr>
<tr><th><tt>application/xslt+xml</tt></th>
<th><tt>xslt</tt></th></tr>
<tr><th><tt>image/svg+xml</tt></th>
<th><tt>svg</tt></th></tr>
<tr><th><tt>image/x-icon</tt></th>
<th><tt>ico</tt></th></tr>
<tr><th><tt>model/vrml</tt></th>
<th><tt>vrml wrl</tt></th></tr>
<tr><th><tt>text/actionscript</tt></th>
<th><tt>actionscript as</tt></th></tr>
<tr><th><tt>text/actionscript3</tt></th>
<th><tt>actionscript3 as3</tt></th></tr>
<tr><th><tt>text/basic</tt></th>
<th><tt>basic qbasic</tt></th></tr>
<tr><th><tt>text/coffeescript</tt></th>
<th><tt>coffee coffee-script coffeescript</tt></th></tr>
<tr><th><tt>text/css</tt></th>
<th><tt>css</tt></th></tr>
<tr><th><tt>text/css+genshi</tt></th>
<th><tt>css+genshi css+genshitext</tt></th></tr>
<tr><th><tt>text/css+jinja</tt></th>
<th><tt>css+django css+jinja</tt></th></tr>
<tr><th><tt>text/css+lasso</tt></th>
<th><tt>css+lasso</tt></th></tr>
<tr><th><tt>text/css+mako</tt></th>
<th><tt>css+mako</tt></th></tr>
<tr><th><tt>text/css+myghty</tt></th>
<th><tt>css+myghty</tt></th></tr>
<tr><th><tt>text/css+php</tt></th>
<th><tt>css+php</tt></th></tr>
<tr><th><tt>text/css+ruby</tt></th>
<th><tt>css+erb css+ruby</tt></th></tr>
<tr><th><tt>text/css+smarty</tt></th>
<th><tt>css+smarty</tt></th></tr>
<tr><th><tt>text/gettext</tt></th>
<th><tt>po pot</tt></th></tr>
<tr><th><tt>text/html</tt></th>
<th><tt>htm</tt></th></tr>
<tr><th><tt>text/html+evoque</tt></th>
<th><tt>html+evoque</tt></th></tr>
<tr><th><tt>text/html+genshi</tt></th>
<th><tt>html+genshi html+kid</tt></th></tr>
<tr><th><tt>text/html+jinja</tt></th>
<th><tt>html+django html+jinja htmldjango</tt></th></tr>
<tr><th><tt>text/html+mako</tt></th>
<th><tt>html+mako</tt></th></tr>
<tr><th><tt>text/html+myghty</tt></th>
<th><tt>html+myghty</tt></th></tr>
<tr><th><tt>text/html+ruby</tt></th>
<th><tt>html+erb html+ruby rhtml</tt></th></tr>
<tr><th><tt>text/html+smarty</tt></th>
<th><tt>html+smarty</tt></th></tr>
<tr><th><tt>text/html+spitfire</tt></th>
<th><tt>html+cheetah html+spitfire htmlcheetah</tt></th></tr>
<tr><th><tt>text/html+twig</tt></th>
<th><tt>html+twig</tt></th></tr>
<tr><th><tt>text/html+velocity</tt></th>
<th><tt>html+velocity</tt></th></tr>
<tr><th><tt>text/idl</tt></th>
<th><tt>idl</tt></th></tr>
<tr><th><tt>text/inf</tt></th>
<th><tt>cfg dosini ini</tt></th></tr>
<tr><th><tt>text/ipf</tt></th>
<th><tt>igor igorpro</tt></th></tr>
<tr><th><tt>text/javascript</tt></th>
<th><tt>javascript js</tt></th></tr>
<tr><th><tt>text/javascript+genshi</tt></th>
<th><tt>javascript+genshi javascript+genshitext js+genshi js+genshitext</tt></th></tr>
<tr><th><tt>text/javascript+jinja</tt></th>
<th><tt>javascript+django javascript+jinja js+django js+jinja</tt></th></tr>
<tr><th><tt>text/javascript+lasso</tt></th>
<th><tt>javascript+lasso js+lasso</tt></th></tr>
<tr><th><tt>text/javascript+mako</tt></th>
<th><tt>javascript+mako js+mako</tt></th></tr>
<tr><th><tt>text/javascript+mygthy</tt></th>
<th><tt>javascript+myghty js+myghty</tt></th></tr>
<tr><th><tt>text/javascript+php</tt></th>
<th><tt>javascript+php js+php</tt></th></tr>
<tr><th><tt>text/javascript+ruby</tt></th>
<th><tt>javascript+erb javascript+ruby js+erb js+ruby</tt></th></tr>
<tr><th><tt>text/javascript+smarty</tt></th>
<th><tt>javascript+smarty js+smarty</tt></th></tr>
<tr><th><tt>text/javascript+spitfire</tt></th>
<th><tt>javascript+cheetah javascript+spitfire js+cheetah js+spitfire</tt></th></tr>
<tr><th><tt>text/jsgf</tt></th>
<th><tt>jsgf</tt></th></tr>
<tr><th><tt>text/juttle</tt></th>
<th><tt>juttle</tt></th></tr>
<tr><th><tt>text/limbo</tt></th>
<th><tt>limbo</tt></th></tr>
<tr><th><tt>text/livescript</tt></th>
<th><tt>live-script livescript</tt></th></tr>
<tr><th><tt>text/matlab</tt></th>
<th><tt>matlab</tt></th></tr>
<tr><th><tt>text/ncl</tt></th>
<th><tt>ncl</tt></th></tr>
<tr><th><tt>text/octave</tt></th>
<th><tt>octave</tt></th></tr>
<tr><th><tt>text/odin</tt></th>
<th><tt>odin</tt></th></tr>
<tr><th><tt>text/plain</tt></th>
<th><tt>AUTHORS COPYING ChangeLog INSTALL README RELEASE TXT text txt</tt></th></tr>
<tr><th><tt>text/prs.fallenstein.rst</tt></th>
<th><tt>rest restructuredtext rst</tt></th></tr>
<tr><th><tt>text/rsl</tt></th>
<th><tt>rsl</tt></th></tr>
<tr><th><tt>text/rust</tt></th>
<th><tt>rust</tt></th></tr>
<tr><th><tt>text/scilab</tt></th>
<th><tt>scilab</tt></th></tr>
<tr><th><tt>text/smali</tt></th>
<th><tt>smali</tt></th></tr>
<tr><th><tt>text/supercollider</tt></th>
<th><tt>sc supercollider</tt></th></tr>
<tr><th><tt>text/swig</tt></th>
<th><tt>swig</tt></th></tr>
<tr><th><tt>text/troff</tt></th>
<th><tt>groff man nroff</tt></th></tr>
<tr><th><tt>text/x-abap</tt></th>
<th><tt>abap</tt></th></tr>
<tr><th><tt>text/x-abnf</tt></th>
<th><tt>abnf</tt></th></tr>
<tr><th><tt>text/x-ada</tt></th>
<th><tt>ada ada2005 ada95 adb ads</tt></th></tr>
<tr><th><tt>text/x-agda</tt></th>
<th><tt>agda</tt></th></tr>
<tr><th><tt>text/x-alloy</tt></th>
<th><tt>alloy</tt></th></tr>
<tr><th><tt>text/x-ambienttalk</tt></th>
<th><tt>ambienttalk ambienttalk/2 at</tt></th></tr>
<tr><th><tt>text/x-apacheconf</tt></th>
<th><tt>aconf apache apacheconf</tt></th></tr>
<tr><th><tt>text/x-arduino</tt></th>
<th><tt>arduino</tt></th></tr>
<tr><th><tt>text/x-asp</tt></th>
<th><tt>asp</tt></th></tr>
<tr><th><tt>text/x-aspectj</tt></th>
<th><tt>aspectj</tt></th></tr>
<tr><th><tt>text/x-asymptote</tt></th>
<th><tt>asy asymptote</tt></th></tr>
<tr><th><tt>text/x-autohotkey</tt></th>
<th><tt>ahk autohotkey</tt></th></tr>
<tr><th><tt>text/x-autoit</tt></th>
<th><tt>autoit</tt></th></tr>
<tr><th><tt>text/x-bb</tt></th>
<th><tt>b3d blitzbasic bplus</tt></th></tr>
<tr><th><tt>text/x-bbcode</tt></th>
<th><tt>bbcode</tt></th></tr>
<tr><th><tt>text/x-bibtex</tt></th>
<th><tt>bib bibtex</tt></th></tr>
<tr><th><tt>text/x-bmx</tt></th>
<th><tt>blitzmax bmax</tt></th></tr>
<tr><th><tt>text/x-bnf</tt></th>
<th><tt>bnf</tt></th></tr>
<tr><th><tt>text/x-boo</tt></th>
<th><tt>boo</tt></th></tr>
<tr><th><tt>text/x-c++hdr</tt></th>
<th><tt>H HH c++hdr hh hpp</tt></th></tr>
<tr><th><tt>text/x-c++src</tt></th>
<th><tt>C C++ CC c++ c++src cc cpp</tt></th></tr>
<tr><th><tt>text/x-c-objdump</tt></th>
<th><tt>c-objdump</tt></th></tr>
<tr><th><tt>text/x-ceylon</tt></th>
<th><tt>ceylon</tt></th></tr>
<tr><th><tt>text/x-chdr</tt></th>
<th><tt>chdr h</tt></th></tr>
<tr><th><tt>text/x-cirru</tt></th>
<th><tt>cirru</tt></th></tr>
<tr><th><tt>text/x-clay</tt></th>
<th><tt>clay</tt></th></tr>
<tr><th><tt>text/x-cmake</tt></th>
<th><tt>cmake</tt></th></tr>
<tr><th><tt>text/x-cobol</tt></th>
<th><tt>cobol</tt></th></tr>
<tr><th><tt>text/x-common-lisp</tt></th>
<th><tt>cl common-lisp lisp</tt></th></tr>
<tr><th><tt>text/x-component-pascal</tt></th>
<th><tt>componentpascal cp</tt></th></tr>
<tr><th><tt>text/x-coq</tt></th>
<th><tt>coq</tt></th></tr>
<tr><th><tt>text/x-cpp-objdump</tt></th>
<th><tt>c++-objdumb cpp-objdump cxx-objdump</tt></th></tr>
<tr><th><tt>text/x-crocsrc</tt></th>
<th><tt>croc</tt></th></tr>
<tr><th><tt>text/x-cryptol</tt></th>
<th><tt>cry cryptol</tt></th></tr>
<tr><th><tt>text/x-crystal</tt></th>
<th><tt>cr crystal</tt></th></tr>
<tr><th><tt>text/x-csharp</tt></th>
<th><tt>C# c# cs csharp</tt></th></tr>
<tr><th><tt>text/x-csrc</tt></th>
<th><tt>c csrc xs</tt></th></tr>
<tr><th><tt>text/x-cuda</tt></th>
<th><tt>cu cuda</tt></th></tr>
<tr><th><tt>text/x-d-objdump</tt></th>
<th><tt>d-objdump</tt></th></tr>
<tr><th><tt>text/x-dart</tt></th>
<th><tt>dart</tt></th></tr>
<tr><th><tt>text/x-dg</tt></th>
<th><tt>dg</tt></th></tr>
<tr><th><tt>text/x-diff</tt></th>
<th><tt>patch</tt></th></tr>
<tr><th><tt>text/x-dockerfile-config</tt></th>
<th><tt>docker dockerfile</tt></th></tr>
<tr><th><tt>text/x-dsrc</tt></th>
<th><tt>d</tt></th></tr>
<tr><th><tt>text/x-dylan</tt></th>
<th><tt>dylan</tt></th></tr>
<tr><th><tt>text/x-dylan-console</tt></th>
<th><tt>dylan-console dylan-repl</tt></th></tr>
<tr><th><tt>text/x-dylan-lid</tt></th>
<th><tt>dylan-lid lid</tt></th></tr>
<tr><th><tt>text/x-earl-grey</tt></th>
<th><tt>earl-grey earlgrey eg</tt></th></tr>
<tr><th><tt>text/x-easytrieve</tt></th>
<th><tt>easytrieve</tt></th></tr>
<tr><th><tt>text/x-ebnf</tt></th>
<th><tt>ebnf</tt></th></tr>
<tr><th><tt>text/x-ecsrc</tt></th>
<th><tt>ec</tt></th></tr>
<tr><th><tt>text/x-eiffel</tt></th>
<th><tt>e eiffel</tt></th></tr>
<tr><th><tt>text/x-elisp</tt></th>
<th><tt>el</tt></th></tr>
<tr><th><tt>text/x-elixir</tt></th>
<th><tt>elixir ex exs</tt></th></tr>
<tr><th><tt>text/x-elixir-shellsession</tt></th>
<th><tt>iex</tt></th></tr>
<tr><th><tt>text/x-elm</tt></th>
<th><tt>elm</tt></th></tr>
<tr><th><tt>text/x-erl-shellsession</tt></th>
<th><tt>erl</tt></th></tr>
<tr><th><tt>text/x-erlang</tt></th>
<th><tt>erlang</tt></th></tr>
<tr><th><tt>text/x-ezhil</tt></th>
<th><tt>ezhil</tt></th></tr>
<tr><th><tt>text/x-factor</tt></th>
<th><tt>factor</tt></th></tr>
<tr><th><tt>text/x-fancysrc</tt></th>
<th><tt>fancy fy</tt></th></tr>
<tr><th><tt>text/x-felix</tt></th>
<th><tt>felix flx</tt></th></tr>
<tr><th><tt>text/x-flatline</tt></th>
<th><tt>flatline</tt></th></tr>
<tr><th><tt>text/x-fortran</tt></th>
<th><tt>f fortran</tt></th></tr>
<tr><th><tt>text/x-fsharp</tt></th>
<th><tt>fsharp</tt></th></tr>
<tr><th><tt>text/x-gas</tt></th>
<th><tt>asm gas</tt></th></tr>
<tr><th><tt>text/x-genshi</tt></th>
<th><tt>genshitext</tt></th></tr>
<tr><th><tt>text/x-gherkin</tt></th>
<th><tt>cucumber gherkin</tt></th></tr>
<tr><th><tt>text/x-glslsrc</tt></th>
<th><tt>glsl</tt></th></tr>
<tr><th><tt>text/x-gnuplot</tt></th>
<th><tt>gnuplot</tt></th></tr>
<tr><th><tt>text/x-gooddata-cl</tt></th>
<th><tt>gooddata-cl</tt></th></tr>
<tr><th><tt>text/x-gosrc</tt></th>
<th><tt>go</tt></th></tr>
<tr><th><tt>text/x-gosu</tt></th>
<th><tt>gosu</tt></th></tr>
<tr><th><tt>text/x-gosu-template</tt></th>
<th><tt>gst</tt></th></tr>
<tr><th><tt>text/x-groovy</tt></th>
<th><tt>groovy</tt></th></tr>
<tr><th><tt>text/x-haml</tt></th>
<th><tt>haml</tt></th></tr>
<tr><th><tt>text/x-handlebars-template</tt></th>
<th><tt>html+handlebars</tt></th></tr>
<tr><th><tt>text/x-haskell</tt></th>
<th><tt>haskell hs</tt></th></tr>
<tr><th><tt>text/x-hsail</tt></th>
<th><tt>hsa hsail</tt></th></tr>
<tr><th><tt>text/x-hx</tt></th>
<th><tt>haxe hx hxsl</tt></th></tr>
<tr><th><tt>text/x-idl</tt></th>
<th><tt>ice</tt></th></tr>
<tr><th><tt>text/x-idris</tt></th>
<th><tt>idr idris</tt></th></tr>
<tr><th><tt>text/x-inf</tt></th>
<th><tt>inf</tt></th></tr>
<tr><th><tt>text/x-iokesrc</tt></th>
<th><tt>ik ioke</tt></th></tr>
<tr><th><tt>text/x-iosrc</tt></th>
<th><tt>io</tt></th></tr>
<tr><th><tt>text/x-irclog</tt></th>
<th><tt>irc</tt></th></tr>
<tr><th><tt>text/x-isabelle</tt></th>
<th><tt>isabelle</tt></th></tr>
<tr><th><tt>text/x-j</tt></th>
<th><tt>j</tt></th></tr>
<tr><th><tt>text/x-jade</tt></th>
<th><tt>jade pug</tt></th></tr>
<tr><th><tt>text/x-java</tt></th>
<th><tt>java</tt></th></tr>
<tr><th><tt>text/x-java-properties</tt></th>
<th><tt>jproperties properties</tt></th></tr>
<tr><th><tt>text/x-jbst</tt></th>
<th><tt>duel jbst jsonml+bst</tt></th></tr>
<tr><th><tt>text/x-jcl</tt></th>
<th><tt>jcl</tt></th></tr>
<tr><th><tt>text/x-kconfig</tt></th>
<th><tt>kconfig kernel-config linux-config menuconfig</tt></th></tr>
<tr><th><tt>text/x-koka</tt></th>
<th><tt>koka</tt></th></tr>
<tr><th><tt>text/x-kotlin</tt></th>
<th><tt>kotlin</tt></th></tr>
<tr><th><tt>text/x-lasso</tt></th>
<th><tt>lasso lassoscript</tt></th></tr>
<tr><th><tt>text/x-latex</tt></th>
<th><tt>latex tex</tt></th></tr>
<tr><th><tt>text/x-lean</tt></th>
<th><tt>lean</tt></th></tr>
<tr><th><tt>text/x-less-css</tt></th>
<th><tt>less</tt></th></tr>
<tr><th><tt>text/x-lighttpd-conf</tt></th>
<th><tt>lighttpd lighty</tt></th></tr>
<tr><th><tt>text/x-literate-agda</tt></th>
<th><tt>lagda literate-agda</tt></th></tr>
<tr><th><tt>text/x-literate-cryptol</tt></th>
<th><tt>lcry lcryptol literate-cryptol</tt></th></tr>
<tr><th><tt>text/x-literate-haskell</tt></th>
<th><tt>lhaskell lhs literate-haskell</tt></th></tr>
<tr><th><tt>text/x-literate-idris</tt></th>
<th><tt>lidr lidris literate-idris</tt></th></tr>
<tr><th><tt>text/x-llvm</tt></th>
<th><tt>llvm</tt></th></tr>
<tr><th><tt>text/x-logos</tt></th>
<th><tt>logos</tt></th></tr>
<tr><th><tt>text/x-logtalk</tt></th>
<th><tt>logtalk</tt></th></tr>
<tr><th><tt>text/x-lsl</tt></th>
<th><tt>lsl</tt></th></tr>
<tr><th><tt>text/x-m4</tt></th>
<th><tt>m4</tt></th></tr>
<tr><th><tt>text/x-mail</tt></th>
<th><tt>mail</tt></th></tr>
<tr><th><tt>text/x-makefile</tt></th>
<th><tt>GNUMakefile Makefile bsdmake make makefile mf mk</tt></th></tr>
<tr><th><tt>text/x-markdown</tt></th>
<th><tt>md</tt></th></tr>
<tr><th><tt>text/x-mask</tt></th>
<th><tt>mask</tt></th></tr>
<tr><th><tt>text/x-minidsrc</tt></th>
<th><tt>minid</tt></th></tr>
<tr><th><tt>text/x-modelica</tt></th>
<th><tt>modelica</tt></th></tr>
<tr><th><tt>text/x-modula2</tt></th>
<th><tt>m2 modula2</tt></th></tr>
<tr><th><tt>text/x-monkey</tt></th>
<th><tt>monkey</tt></th></tr>
<tr><th><tt>text/x-moocode</tt></th>
<th><tt>moo moocode</tt></th></tr>
<tr><th><tt>text/x-mql</tt></th>
<th><tt>mq4 mq5 mql mql4 mql5</tt></th></tr>
<tr><th><tt>text/x-mysql</tt></th>
<th><tt>mysql</tt></th></tr>
<tr><th><tt>text/x-nasm</tt></th>
<th><tt>nasm</tt></th></tr>
<tr><th><tt>text/x-nasm-objdump</tt></th>
<th><tt>objdump-nasm</tt></th></tr>
<tr><th><tt>text/x-nemerle</tt></th>
<th><tt>nemerle</tt></th></tr>
<tr><th><tt>text/x-nescsrc</tt></th>
<th><tt>nesc</tt></th></tr>
<tr><th><tt>text/x-newspeak</tt></th>
<th><tt>newspeak</tt></th></tr>
<tr><th><tt>text/x-nginx-conf</tt></th>
<th><tt>nginx nginx-conf</tt></th></tr>
<tr><th><tt>text/x-nim</tt></th>
<th><tt>nim nimrod</tt></th></tr>
<tr><th><tt>text/x-nix</tt></th>
<th><tt>nix nixos</tt></th></tr>
<tr><th><tt>text/x-nsis</tt></th>
<th><tt>nsh nsi nsis</tt></th></tr>
<tr><th><tt>text/x-objc</tt></th>
<th><tt>m mm</tt></th></tr>
<tr><th><tt>text/x-objdump</tt></th>
<th><tt>objdump</tt></th></tr>
<tr><th><tt>text/x-objective-c</tt></th>
<th><tt>obj-c objc objective-c objectivec</tt></th></tr>
<tr><th><tt>text/x-objective-c++</tt></th>
<th><tt>obj-c++ objc++ objective-c++ objectivec++</tt></th></tr>
<tr><th><tt>text/x-objective-j</tt></th>
<th><tt>obj-j objective-j objectivej objj</tt></th></tr>
<tr><th><tt>text/x-ocaml</tt></th>
<th><tt>ml mli ocaml</tt></th></tr>
<tr><th><tt>text/x-ooc</tt></th>
<th><tt>ooc</tt></th></tr>
<tr><th><tt>text/x-opa</tt></th>
<th><tt>opa</tt></th></tr>
<tr><th><tt>text/x-parasail</tt></th>
<th><tt>parasail</tt></th></tr>
<tr><th><tt>text/x-pascal</tt></th>
<th><tt>delphi objectpascal pas pascal</tt></th></tr>
<tr><th><tt>text/x-patch</tt></th>
<th><tt>diff udiff</tt></th></tr>
<tr><th><tt>text/x-pawn</tt></th>
<th><tt>pawn</tt></th></tr>
<tr><th><tt>text/x-perl</tt></th>
<th><tt>PL pm</tt></th></tr>
<tr><th><tt>text/x-php</tt></th>
<th><tt>php php3 php4 php5</tt></th></tr>
<tr><th><tt>text/x-pig</tt></th>
<th><tt>pig</tt></th></tr>
<tr><th><tt>text/x-pike</tt></th>
<th><tt>pike</tt></th></tr>
<tr><th><tt>text/x-plpgsql</tt></th>
<th><tt>plpgsql</tt></th></tr>
<tr><th><tt>text/x-postgresql</tt></th>
<th><tt>postgres postgresql</tt></th></tr>
<tr><th><tt>text/x-postgresql-psql</tt></th>
<th><tt>postgres-console postgresql-console psql</tt></th></tr>
<tr><th><tt>text/x-povray</tt></th>
<th><tt>pov</tt></th></tr>
<tr><th><tt>text/x-powershell</tt></th>
<th><tt>posh powershell ps1 psm1</tt></th></tr>
<tr><th><tt>text/x-prolog</tt></th>
<th><tt>prolog</tt></th></tr>
<tr><th><tt>text/x-psp</tt></th>
<th><tt>psp</tt></th></tr>
<tr><th><tt>text/x-python-doctest</tt></th>
<th><tt>pycon python-doctest</tt></th></tr>
<tr><th><tt>text/x-python-traceback</tt></th>
<th><tt>pytb</tt></th></tr>
<tr><th><tt>text/x-python3-traceback</tt></th>
<th><tt>py3tb</tt></th></tr>
<tr><th><tt>text/x-r-doc</tt></th>
<th><tt>rd</tt></th></tr>
<tr><th><tt>text/x-r-profile</tt></th>
<th><tt>r s splus</tt></th></tr>
<tr><th><tt>text/x-rebol</tt></th>
<th><tt>rebol</tt></th></tr>
<tr><th><tt>text/x-red-system</tt></th>
<th><tt>red red/system</tt></th></tr>
<tr><th><tt>text/x-rexx</tt></th>
<th><tt>arexx rexx</tt></th></tr>
<tr><th><tt>text/x-rfc</tt></th>
<th><tt>rfc</tt></th></tr>
<tr><th><tt>text/x-robotframework</tt></th>
<th><tt>robotframework</tt></th></tr>
<tr><th><tt>text/x-rpm-spec</tt></th>
<th><tt>spec</tt></th></tr>
<tr><th><tt>text/x-rql</tt></th>
<th><tt>rql</tt></th></tr>
<tr><th><tt>text/x-ruby-shellsession</tt></th>
<th><tt>irb rbcon</tt></th></tr>
<tr><th><tt>text/x-sass</tt></th>
<th><tt>sass</tt></th></tr>
<tr><th><tt>text/x-scala</tt></th>
<th><tt>scala</tt></th></tr>
<tr><th><tt>text/x-scaml</tt></th>
<th><tt>scaml</tt></th></tr>
<tr><th><tt>text/x-scss</tt></th>
<th><tt>scss</tt></th></tr>
<tr><th><tt>text/x-slim</tt></th>
<th><tt>slim</tt></th></tr>
<tr><th><tt>text/x-sls</tt></th>
<th><tt>salt sls yaml+jinja</tt></th></tr>
<tr><th><tt>text/x-smalltalk</tt></th>
<th><tt>smalltalk squeak st</tt></th></tr>
<tr><th><tt>text/x-snobol</tt></th>
<th><tt>snobol</tt></th></tr>
<tr><th><tt>text/x-sourcepawn</tt></th>
<th><tt>sp</tt></th></tr>
<tr><th><tt>text/x-sql</tt></th>
<th><tt>sql</tt></th></tr>
<tr><th><tt>text/x-sqlite3-console</tt></th>
<th><tt>sqlite3</tt></th></tr>
<tr><th><tt>text/x-squidconf</tt></th>
<th><tt>squid squid.conf squidconf</tt></th></tr>
<tr><th><tt>text/x-swift</tt></th>
<th><tt>swift</tt></th></tr>
<tr><th><tt>text/x-systemverilog</tt></th>
<th><tt>sv systemverilog</tt></th></tr>
<tr><th><tt>text/x-tasm</tt></th>
<th><tt>tasm</tt></th></tr>
<tr><th><tt>text/x-tea</tt></th>
<th><tt>tea</tt></th></tr>
<tr><th><tt>text/x-textile</tt></th>
<th><tt>textile txtl</tt></th></tr>
<tr><th><tt>text/x-todo</tt></th>
<th><tt>todotxt</tt></th></tr>
<tr><th><tt>text/x-trac-wiki</tt></th>
<th><tt>moin trac-wiki</tt></th></tr>
<tr><th><tt>text/x-tsql</tt></th>
<th><tt>t-sql tsql</tt></th></tr>
<tr><th><tt>text/x-typescript</tt></th>
<th><tt>ts typescript</tt></th></tr>
<tr><th><tt>text/x-typoscript</tt></th>
<th><tt>typoscript</tt></th></tr>
<tr><th><tt>text/x-vala</tt></th>
<th><tt>vala vapi</tt></th></tr>
<tr><th><tt>text/x-vba</tt></th>
<th><tt>bas vb vb.net vba vbnet</tt></th></tr>
<tr><th><tt>text/x-vclsnippet</tt></th>
<th><tt>vclsnippet vclsnippets</tt></th></tr>
<tr><th><tt>text/x-vclsrc</tt></th>
<th><tt>vcl</tt></th></tr>
<tr><th><tt>text/x-verilog</tt></th>
<th><tt>v verilog</tt></th></tr>
<tr><th><tt>text/x-vhdl</tt></th>
<th><tt>vhd vhdl</tt></th></tr>
<tr><th><tt>text/x-vim</tt></th>
<th><tt>vim</tt></th></tr>
<tr><th><tt>text/x-whiley</tt></th>
<th><tt>whiley</tt></th></tr>
<tr><th><tt>text/x-windows-registry</tt></th>
<th><tt>registry</tt></th></tr>
<tr><th><tt>text/x-x10</tt></th>
<th><tt>x10 xten</tt></th></tr>
<tr><th><tt>text/x-xtend</tt></th>
<th><tt>xtend</tt></th></tr>
<tr><th><tt>text/x-yaml</tt></th>
<th><tt>yaml</tt></th></tr></table>



---



See also: [WikiProcessors](wiki-processors), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracBrowser](trac-browser)


