Wiki Phrases
A wiki phrase belongs to one of the categories *fixme*, *todo* or *done*,
and is highlighted to catch attention. Any delimiter `():<>` adjacent to a
phrase will not be presented. This makes it possible to naturally write, for
example, `FIXME:` in a wiki text, but view the phrase highlighted without the
colon (`:`) which would not look natural.

Use the following phrases to highlight problems, todos and results thereof:
BUG BUGFIXME FIXMEREVIEW REVIEWTODO TODODEBUGGED DEBUGGEDDONE DONEFIXED FIXEDREVIEWED REVIEWED
Prefixing a phrase with `!` prevents it from being highlighted.

Example:
 Wiki markup  Display 
FIXME: Something weird is going on
FIXME Something weird is going on 

See also [TracIni](trac-ini#), for configuration of phrases,
and [WikiMacros](wiki-macros#) on how to use the `ShowPhrases`
macro.
