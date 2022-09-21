# dowker_statistics
Tools for applying the Dowker complex statistically

# Tools included

## `dowker_nest`

Input: table with columns:
* Observations (factor variable)
* Features (factor variable)
* Class (factor variable, partially assigned)

Output: new table:
* Column of feature sets (one feature can appear multiple times)
* Column of observation sets (each observation appears exactly once)
* Probability of each class

## Compute Dowker lattice/graph

Nesting of feature sets; identify where inconsistent edges where observation count increases

Translate existing Python code to R.

What R graph library (if any) should we use as a dependency?

Graphics are optional

# Workflows to be supported

## Classification workflow

Goal: Find the probability that a given observation will be assigned a given class

Tool: Dowker probability formula, based upon Bayes' rule

## Explanation workflow

Goal: Determine the "interesting" feature patterns

Tool: Inconsistent edges

## Stretch: Sampling sufficiency test

Goal: Split the observations into non-inconsistent subsets

Tool: Robinson's "Dowker splitting" algorithm

# Example use cases

1. Book data 
2. Star Wars (stretch, due to sampling issues)
3. MPEG/JPEG data might be releasable
4. ICCMax calculator element data might be releasable
5. Byte value statistics from binary executables and/or memory images