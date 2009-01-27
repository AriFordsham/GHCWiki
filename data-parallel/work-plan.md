## Work plan for implementing Data Parallel Haskell

### Categories


Tasks below are labelled with categories that indicate the purpose of the task:

<table><tr><th>*Efficiency*</th>
<td>
Improve scalability and/or baseline performance of generated code
</td></tr>
<tr><th>*Compile time*</th>
<td>
Improve compile times
</td></tr>
<tr><th>*Ease of use*</th>
<td>
Make the system easier or more convenient to use for end users
</td></tr></table>

### Task assignments

<table><tr><th>*Roman*</th>
<td>**Replicate** & **Recycling**
– status: partly implemented, but still needs serious work
</td></tr></table>

<table><tr><th>*Simon*</th>
<td>**Code blow up**
– status: unknown
</td></tr></table>

### Open tasks

1. **Replicate** \[*Efficiency*\]: Implement an extended array representation that uses an optimised representation for arrays that are the result of common forms of replication (i.e., due to free variables in lifted expressions).  The optimised representation stores the data to be replicated and the replication count(s) instead of actually replicating the data.  This also requires all functions consuming arrays to be adapted.

1. **Recycling** \[*Efficiency*\]: Use Roman's recycling optimisation (PADL'09) to avoid copying in `joinD`.

1. **Scaling** \[*Efficiency*\]: Investigate the scaling problems that we are seeing with vectorised code at the moment.  (**Replicate** and **Recycling** play a role here, but it is unclear whether that's all.)

1. **Test new inliner** \[*Compile time* & *Efficiency*\]: Retest package dph with new inliner and the simplifier changes and try to simplify the library on the basis of these new phases.

1. **Code blow up** \[*Compile time*\]: GHC generates a lot of intermediate code when vectorisation is enabled, leading to excessive compilation times.  Find out whether the new inliner helped here and what else can be done to improve this situation.

1. **Conversion of vectorised representations** \[*Ease of use*\]: We need other than just identity conversions between vanilla and vectorised data representations, especially `[:a:] <-> PArray a`.  This will make the system more convenient to use.
