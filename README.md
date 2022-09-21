# dowker_statistics
Tools for applying the Dowker complex statistically

# Tools included

## `dowker_nest`

Input: table with columns:
* Observations (factor variable)
* Features (factor variable)
* Class (factor variable)

Output: new table:
* Column of feature sets (one feature can appear multiple times)
* Column of observation sets (each observation appears exactly once)
* Probability of each class

## Compute Dowker lattice/graph

Nesting of feature sets; identify where inconsistent edges where observation count increases

Translate existing Python code to R.

What R graph library (if any) should we use as a dependency?

Graphics are optional

# Example use cases

1. Book data 
2. Star Wars (stretch, due to sampling issues)
3. MPEG/JPEG data might be releasable
4. ICCMax calculator element data might be releasable
5. Byte value statistics from binary executables and/or memory images