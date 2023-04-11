# dowker_statistics
Tools for applying the Dowker complex statistically

This material is based upon work supported by the Defense Advanced Research 
Projects Agency (DARPA) SafeDocs program under contract HR001119C0072. 
Any opinions, findings and conclusions or recommendations expressed in 
this material are those of the authors and do not necessarily reflect the 
views of DARPA.

## Dependency notes

Most everything can be installed with `install.packages` from CRAN.
The only exception is the `relations.kappa` function, which is from 
[https://github.com/kpewing/relations].  If not desired, the code chunk in
question in `nitf_demo.R` can be skipped.

# Workflows to be supported

## Explanation workflow

Goal: Determine the "interesting" feature patterns

Tool: `dowker_nest`, `pca_generator`

## Classification workflow

Goal: Find the probability that a given observation will be assigned a given class

Tool: `dowker_prob`, `bernoulli_table`

## Unsupervised dialect clustering

Goal: Split the observations into non-inconsistent subsets

Tool: `max_decreasing_decomp`

# Example use cases

* CSV file dialects
* NITF file dialects