# Syntax Coloring of Source Code


Trac supports language-specific syntax highlighting of source code in [wiki formatted](wiki-formatting) text and the [repository browser](trac-browser).


To do this, Trac uses external libraries with support for a great number of programming languages.


Currently Trac supports syntax coloring using one or more of the following packages:

- [ GNU Enscript](http://people.ssh.fi/mtr/genscript/)
- [ SilverCity](http://silvercity.sourceforge.net/)


To activate syntax coloring, simply install either one (or more) of these packages. No additional configuration is required, however to modify the colors, have a look at `trac/htdocs/css/code.css`.


When in use, Trac will automatically prioritize SilverCity highlighting over Enscript if possible, (see note below). 


If neither package is available, Trac will display the data as plain text. 

**Note:** Enscript supports a greater number of languages, however SilverCity is generally faster since it is a library and isn't executed in an external process.

### About SilverCity


SilverCity uses the lexer from [ Scintilla](http://www.scintilla.org/). Scintilla supports more languages than SilverCity implements. If you want to add a language to SilverCity supported by Scintilla, it's not very difficult. See [ SilverCityAddLanguage](http://projects.edgewall.com/trac/wiki/SilverCityAddLanguage) for some information how.

## Syntax Coloring Support

<table><tr><th></th>
<th> SilverCity </th>
<th> Enscript 
</th></tr>
<tr><th> Ada      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Asm      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> \* ASP    </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* C      </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* C++    </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* Java   </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Awk      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> CSS      </th>
<th> X </th>
<th></th></tr>
<tr><th> Diff     </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Eiffel   </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Elisp    </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Fortran  </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Haskell  </th>
<th></th>
<th> X 
</th></tr>
<tr><th> HTML     </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> IDL      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Javascript </th>
<th> X </th>
<th> X 
</th></tr></table>

<table><tr><th> m4       </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Makefile </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Matlab   </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Objective-C</th>
<th></th>
<th> X 
</th></tr>
<tr><th> Pascal   </th>
<th></th>
<th> X 
</th></tr>
<tr><th> \* Perl   </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* PHP    </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> PSP      </th>
<th> X </th>
<th></th></tr>
<tr><th> Pyrex    </th>
<th></th>
<th> X 
</th></tr>
<tr><th> \* Python </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> \* Ruby   </th>
<th> X </th>
<th> X (1) 
</th></tr>
<tr><th> Scheme   </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Shell    </th>
<th></th>
<th> X 
</th></tr>
<tr><th> SQL      </th>
<th> X </th>
<th> X 
</th></tr>
<tr><th> Troff    </th>
<th></th>
<th> X 
</th></tr>
<tr><th> TCL      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Tex      </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Verilog  </th>
<th></th>
<th> X 
</th></tr>
<tr><th> VHDL     </th>
<th></th>
<th> X 
</th></tr>
<tr><th> Visual Basic </th>
<th> X 
</th>
<th></th></tr>
<tr><th> VRML     </th>
<th></th>
<th> X 
</th></tr>
<tr><th> XML      </th>
<th> X </th>
<th> X 
</th></tr></table>

*(\*) Supported as inline code blocks in [Wiki text](wiki-formatting) using [WikiProcessors](wiki-processors).*

*(1) Ruby highlighting is not included in the Enscript distribution.  Highlighting rules for Ruby can be obtained from: [ http://neugierig.org/software/ruby/](http://neugierig.org/software/ruby/)*

## Extra Software

- GNU Enscript -- [ http://people.ssh.fi/mtr/genscript/](http://people.ssh.fi/mtr/genscript/)
- SilverCity -- [ http://silvercity.sf.net/](http://silvercity.sf.net/)

---


See also: [WikiProcessors](wiki-processors), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracBrowser](trac-browser)