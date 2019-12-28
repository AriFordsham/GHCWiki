# Symbol Names


Since Haskell allows many symbols in constructor and variable names that C compilers or assembly might not allow (e.g. `:`, `%`, `#`) these have to be encoded using z-encoding.  The encoding is as follows.  See [compiler/utils/Encoding.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/utils/Encoding.hs) and https://gitlab.haskell.org/ghc/ghc/snippets/1535 for encoding/decoding implementations.

## Tuples

<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> <tt>()</tt>  </th>
<th> Z0T     </th>
<th> Unit / 0-tuple 
</th></tr>
<tr><th>         </th>
<th>         </th>
<th> There is no Z1T 
</th></tr>
<tr><th> <tt>(,)</tt> </th>
<th> Z2T </th>
<th> 2-tuple 
</th></tr>
<tr><th> <tt>(,,)</tt> </th>
<th> Z3T </th>
<th> 3-tuple 
</th></tr>
<tr><th> ... </th>
<th> </th>
<th> And so on 
</th></tr></table>

## Unboxed Tuples

<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th>         </th>
<th>         </th>
<th> There is no Z0H 
</th></tr>
<tr><th> <tt>(# #)</tt> </th>
<th> Z1H  </th>
<th> unboxed 1-tuple (note the space) 
</th></tr>
<tr><th> <tt>(#,#)</tt> </th>
<th> Z2H  </th>
<th> unboxed 2-tuple 
</th></tr>
<tr><th> <tt>(#,,#)</tt> </th>
<th> Z3H  </th>
<th> unboxed 3-tuple 
</th></tr>
<tr><th> ... </th>
<th> </th>
<th> And so on 
</th></tr></table>

## Alphanumeric Characters

<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> a-y, A-Y, 0-9 </th>
<th> a-y, A-Y, 0-9 </th>
<th> Regular letters don&apos;t need escape sequences 
</th></tr>
<tr><th> z, Z </th>
<th> zz, ZZ </th>
<th> &apos;Z&apos; and &apos;z&apos; must be escaped 
</th></tr></table>


## Constructor Characters


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> <tt>(</tt> </th>
<th> ZL </th>
<th> Left 
</th></tr>
<tr><th> <tt>)</tt> </th>
<th> ZR </th>
<th> Right 
</th></tr>
<tr><th> <tt>[</tt> </th>
<th> ZM </th>
<th> &apos;M&apos; before &apos;N&apos; in [] 
</th></tr>
<tr><th> <tt>]</tt> </th>
<th> ZN </th>
<th> 
</th></tr>
<tr><th> <tt>:</tt> </th>
<th> ZC </th>
<th> Colon 
</th></tr></table>

## Variable Characters

<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Mnemonic 
</th></tr>
<tr><th> <tt>&</tt> </th>
<th> za </th>
<th> Ampersand 
</th></tr>
<tr><th> <tt>|</tt> </th>
<th> zb </th>
<th> Bar 
</th></tr>
<tr><th> <tt>^</tt> </th>
<th> zc </th>
<th> Caret 
</th></tr>
<tr><th> <tt>$</tt> </th>
<th> zd </th>
<th> Dollar 
</th></tr>
<tr><th> <tt>=</tt> </th>
<th> ze </th>
<th> Equals 
</th></tr>
<tr><th> <tt>></tt> </th>
<th> zg </th>
<th> Greater than 
</th></tr>
<tr><th> <tt>#</tt> </th>
<th> zh </th>
<th> Hash 
</th></tr>
<tr><th> <tt>.</tt> </th>
<th> zi </th>
<th> The dot of the &apos;i&apos; 
</th></tr>
<tr><th> <tt><</tt> </th>
<th> zl </th>
<th> Less than 
</th></tr>
<tr><th> <tt>-</tt> </th>
<th> zm </th>
<th> Minus 
</th></tr>
<tr><th> <tt>!</tt> </th>
<th> zn </th>
<th> Not 
</th></tr>
<tr><th> <tt>+</tt> </th>
<th> zp </th>
<th> Plus 
</th></tr>
<tr><th> <tt>'</tt> </th>
<th> zq </th>
<th> Quote 
</th></tr>
<tr><th> <tt>\</tt> </th>
<th> zr </th>
<th> Reverse slash 
</th></tr>
<tr><th> <tt>/</tt> </th>
<th> zs </th>
<th> Slash 
</th></tr>
<tr><th> <tt>*</tt> </th>
<th> zt </th>
<th> Times sign 
</th></tr>
<tr><th> <tt>_</tt> </th>
<th> zu </th>
<th> Underscore 
</th></tr>
<tr><th> <tt>%</tt> </th>
<th> zv </th>
<th> (TODO I don&apos;t know what the mnemonic for this one is. Perhaps relatiVe or diVide?) 
</th></tr></table>


## Other


Any other character is encoded as a 'z' followed by its hex code (lower case, variable length) followed by 'U'.  If the hex code starts with 'a', 'b, 'c', 'd', 'e' or 'f', then an extra '0' is placed before the hex code to avoid conflicts with the other escape characters.

## Examples

<table><tr><th> Before       </th>
<th> After 
</th></tr>
<tr><th> <tt>Trak</tt>      </th>
<th> <tt>Trak</tt> 
</th></tr>
<tr><th> <tt>foo_wib</tt> </th>
<th> <tt>foozuwib</tt> 
</th></tr>
<tr><th> <tt>></tt>          </th>
<th> <tt>zg</tt> 
</th></tr>
<tr><th> <tt>>1</tt>        </th>
<th> <tt>zg1</tt> 
</th></tr>
<tr><th> <tt>foo#</tt>     </th>
<th> <tt>foozh</tt> 
</th></tr>
<tr><th> <tt>foo##</tt>   </th>
<th> <tt>foozhzh</tt> 
</th></tr>
<tr><th> <tt>foo##1</tt> </th>
<th> <tt>foozhzh1</tt> 
</th></tr>
<tr><th> <tt>fooZ</tt>     </th>
<th> <tt>fooZZ</tt> 
</th></tr>
<tr><th> <tt>:+</tt>        </th>
<th> <tt>ZCzp</tt> 
</th></tr>
<tr><th> <tt>()</tt>          </th>
<th> <tt>Z0T</tt> 
</th></tr>
<tr><th> <tt>(,,,,)</tt>      </th>
<th> <tt>Z5T</tt> 
</th></tr>
<tr><th> <tt>(# #)</tt>     </th>
<th> <tt>Z1H</tt> 
</th></tr>
<tr><th> <tt>(#,,,,#)</tt>  </th>
<th> <tt>Z5H</tt> 
</th></tr></table>


