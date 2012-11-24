## OS X 32bit vs 64bit


Currently, some people use the 32bit version of GHC on OS X for performance reasons. It's not clear for how much longer this will be viable, as other OS X libraries start dropping 32bit support.


Full nofib results comparing the two are
[ here for static by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-base.html), and
[ here for dynamic by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-dyn.html), but the highlights are in the table below.


The left-hand column shows the status quo: 64bit only beats 32bit in mutator time, and that is a shallow victory as the higher GC time means that total runtime is worse for 64bit.


The right-hand column shows what the situation would be if we switch to dynamic instead. Allocations, memory use etc remain higher due to all word-sized things being twice as big. However, the speed difference moves in 64bit's favour, and 64bit is now faster overall.

<table><tr><th></th>
<th>x86 -\> x86_64
when static by default</th>
<th>x86 -\> x86_64
when dynamic by default</th></tr>
<tr><th>Binary Sizes</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+38.0%</th>
<th>+7.4%</th></tr>
<tr><th>+1 s.d.</th>
<th>+38.6%</th>
<th>+30.6%</th></tr>
<tr><th>Average</th>
<th>+38.3%</th>
<th>+18.5%</th></tr>
<tr><th>Allocations</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+63.2%</th>
<th>+63.2%</th></tr>
<tr><th>+1 s.d.</th>
<th>+114.4%</th>
<th>+114.4%</th></tr>
<tr><th>Average</th>
<th>+87.0%</th>
<th>+87.0%</th></tr>
<tr><th>Run Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-23.5%</th>
<th>-31.6%</th></tr>
<tr><th>+1 s.d.</th>
<th>+36.1%</th>
<th>+14.7%</th></tr>
<tr><th>Average</th>
<th>+2.1%</th>
<th>-11.4%</th></tr>
<tr><th>Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-18.2%</th>
<th>-30.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>+40.1%</th>
<th>+17.0%</th></tr>
<tr><th>Average</th>
<th>+7.0%</th>
<th>-9.5%</th></tr>
<tr><th>Mutator Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-32.4%</th>
<th>-38.8%</th></tr>
<tr><th>+1 s.d.</th>
<th>+20.1%</th>
<th>+3.0%</th></tr>
<tr><th>Average</th>
<th>-9.9%</th>
<th>-20.6%</th></tr>
<tr><th>Mutator Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-28.7%</th>
<th>-37.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+22.5%</th>
<th>+4.4%</th></tr>
<tr><th>Average</th>
<th>-6.6%</th>
<th>-19.5%</th></tr>
<tr><th>GC Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+4.5%</th>
<th>-11.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+74.8%</th>
<th>+54.1%</th></tr>
<tr><th>Average</th>
<th>+35.2%</th>
<th>+16.5%</th></tr>
<tr><th>GC Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+7.9%</th>
<th>-8.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>+75.1%</th>
<th>+56.7%</th></tr>
<tr><th>Average</th>
<th>+37.4%</th>
<th>+20.0%</th></tr>
<tr><th>Total Memory in use</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.7%</th>
<th>-1.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+88.9%</th>
<th>+88.9%</th></tr>
<tr><th>Average</th>
<th>+36.3%</th>
<th>+36.1%</th></tr>
<tr><th>Compile Times</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+11.9%</th>
<th>-8.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+21.1%</th>
<th>+2.9%</th></tr>
<tr><th>Average</th>
<th>+16.4%</th>
<th>-3.1%</th></tr></table>