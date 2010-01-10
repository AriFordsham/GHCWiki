# Syntax Coloring of Source Code


Trac supports language-specific syntax highlighting of source code in [wiki formatted](wiki-formatting) text and the [repository browser](trac-browser).


To do this, Trac uses external libraries with support for a great number of programming languages.


Currently Trac supports syntax coloring using one or more of the following packages:

- [ GNU Enscript](http://www.codento.com/people/mtr/genscript/)
- [ SilverCity](http://silvercity.sourceforge.net/)
- [ Pygments](http://pygments.pocoo.org/) (but needs a plugin for 0.10, see [below](trac-syntax-coloring#about-pygments))


To activate syntax coloring, simply install either one (or more) of these packages. No additional configuration is required, however to modify the colors, have a look at `trac/htdocs/css/code.css`.


If you don't know why trac isnt detecting an installed library, try turning on Trac logging. It actually appears that Trac is broken with SilverCity 0.9.6. Either use the current 0.9.7 or the older 0.9.5 [ http://trac.edgewall.org/wiki/TracFaq\#why-is-my-css-code-not-being-highlighted-even-though-i-have-silvercity-installed](http://trac.edgewall.org/wiki/TracFaq#why-is-my-css-code-not-being-highlighted-even-though-i-have-silvercity-installed)


When in use, Trac will automatically prioritize SilverCity highlighting over Enscript if possible, (see note below). 


If neither package is available, Trac will display the data as plain text. 

**Note:** Enscript supports a greater number of languages, however SilverCity is generally faster since it is a library and isn't executed in an external process.

### About SilverCity


SilverCity uses the lexer from [ Scintilla](http://www.scintilla.org/). Scintilla supports more languages than SilverCity implements. If you want to add a language to SilverCity supported by Scintilla, it's not very difficult. See [ SilverCityAddLanguage](http://trac.edgewall.org/wiki/SilverCityAddLanguage) for some information how.

### About Pygments


Starting with trac 0.11 [ pygments](http://pygments.org/) will be the new default highlighter. It's a highlighting library implemented in pure python, very fast, easy to extend and [ well documented](http://pygments.org/docs/). While it does not support as many languages as Enscript or Scintilla the overall output quality is much better.


To use pygments in trac 0.11 you just have to install pygments 0.6 or higher.  If you want to use it in trac 0.10 too you have to install the \[th:TracPygmentsPlugin TracPygmentsPlugin\] from trac hacks. If you encounter any bugs, please file tickets regarding Pygments at [ the Pygments Trac site](http://dev.pocoo.org/projects/pygments/).


The Pygments default style can specified in the [mime-viewer](trac-ini#) section of trac.ini. The default style can be over-ridden by setting a Style preference on the [preferences page](/trac/ghc/prefs/pygments). 


It's very likely that the list below is outdated because the list of supported pygments lexers is growing weekly. Just have a look at the page of [ supported lexers](http://pygments.org/docs/lexers/) on the pygments webpage.

## Syntax Coloring Support

<table><tr><th></th>
<th> SilverCity </th>
<th> Enscript </th>
<th> Pygments 
</th></tr>
<tr><th> Ada      </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Asm      </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Apache Conf (htaccess)) </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> \* ASP    </th>
<th> X </th>
<th> X </th>
<th></th></tr>
<tr><th> \* C      </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* C\#     </th>
<th></th>
<th> X(2) </th>
<th> X 
</th></tr>
<tr><th> \* C++    </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* Java   </th>
<th> X(4)</th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Awk      </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Boo </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> CSS      </th>
<th> X </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Python Doctests </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> Diff     </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Eiffel   </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Elisp    </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Fortran  </th>
<th></th>
<th> X(3) </th>
<th> X 
</th></tr>
<tr><th> Haskell  </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Genshi </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> HTML     </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> IDL      </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> INI </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> Javascript </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Lua </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> m4       </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Makefile </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Mako </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> Matlab   </th>
<th></th>
<th> X (5) </th>
<th></th></tr>
<tr><th> Mygthy </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> Objective-C</th>
<th></th>
<th> X </th>
<th>X 
</th></tr>
<tr><th> OCaml    </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> Pascal   </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* Perl   </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* PHP    </th>
<th> X </th>
<th></th>
<th> X 
</th></tr>
<tr><th> PSP      </th>
<th> X </th>
<th></th>
<th></th></tr>
<tr><th> Pyrex    </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> \* Python </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* Ruby   </th>
<th> X </th>
<th> X (1) </th>
<th> X 
</th></tr>
<tr><th> Scheme   </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Shell    </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Smarty </th>
<th></th>
<th></th>
<th> X 
</th></tr>
<tr><th> SQL      </th>
<th> X </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Troff    </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> TCL      </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Tex      </th>
<th></th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Verilog  </th>
<th> X(4) </th>
<th> X </th>
<th></th></tr>
<tr><th> VHDL     </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> Visual Basic </th>
<th> X </th>
<th> X 
</th>
<th></th></tr>
<tr><th> VRML     </th>
<th></th>
<th> X </th>
<th></th></tr>
<tr><th> XML      </th>
<th> X </th>
<th></th>
<th> X 
</th></tr></table>

*(\*) Supported as inline code blocks in [Wiki text](wiki-formatting) using [WikiProcessors](wiki-processors).*

*(1) Ruby highlighting is not included in the Enscript distribution.  Highlighting rules for Ruby can be obtained from: [ http://neugierig.org/software/ruby/](http://neugierig.org/software/ruby/)*

*(2) C\# highlighting is not included in the Enscript distribution.  Highlighting rules for C\# can be obtained from: [ http://wiki.hasno.info/index.php/Csharp.st](http://wiki.hasno.info/index.php/Csharp.st)*

*(3) Fortran: as packaged, Enscript only supports the fixed source form. Highlighting rules for Fortran 90x/2003 can be obtained from: [ http://wiki.hasno.info/index.php/F90.st](http://wiki.hasno.info/index.php/F90.st)*

*(4) since Silvercity 0.9.7 released on 2006-11-23
*

*(5) By default `.m` files are considered Objective-C files. In order to treat `.m` files as MATLAB files, add "text/x-matlab:m" to the "mime_map" setting in the [\[mimeviewer\] section of trac.ini](trac-ini#).
*

## Extra Software

- GNU Enscript -- [ http://directory.fsf.org/GNU/enscript.html](http://directory.fsf.org/GNU/enscript.html)
- GNU Enscript for Windows -- [ http://gnuwin32.sourceforge.net/packages/enscript.htm](http://gnuwin32.sourceforge.net/packages/enscript.htm)
- SilverCity -- [ http://silvercity.sf.net/](http://silvercity.sf.net/)
- Pygments -- [ http://pygments.org/](http://pygments.org/)

---


See also: [WikiProcessors](wiki-processors), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracBrowser](trac-browser)