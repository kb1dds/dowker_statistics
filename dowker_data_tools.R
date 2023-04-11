# Tools for manipulating data from the DARPA SafeDocs project
#
#Copyright (c) 2023 Michael Robinson
#
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use 
# this file except in compliance with the License. You may obtain a copy of the 
#License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#License for the specific language governing permissions and limitations under
# the License.

library(tidyverse)
library(reticulate)
np<-import('numpy') # Needed to parse the npz files from BAE

## Reader for data
# Note to self: the npz files used on this project represent sparse Boolean matrices
# The column indices (files being tested) are given by the `indices` array
# The entries (typically all `1`s) are given by the `data` array
# The row indices (error regexes) are given "inverse" in the indptr array,
#  which is to say that each entry of the indptr array corresponds to a row,
#  and the value of that entry tells the max entry in the data in that row
# Output: 
#  A list of three items describing the data:
#   $matrix: a tibble in which each row is an instance of a file triggering a regex, with columns
#    `file` : which file ID is observed
#    `regex` : which regex is observed
#    `error` : logical TRUE (implies regex was triggered)
#    `index` : integer row number
#   $nfiles: integer number of files appearing in $matrix
#   $nregexes integer number of regexes appearing in $matrix
npz_error_matrix_reader <- function(filename){
  reader<-np$load(filename)
  
  nfiles <- reader$f[['shape']][2]
  nregexes <- reader$f[['shape']][1]
  
  errors <- tibble(file=reader$f[['indices']],error=reader$f[['data']]) %>% 
    mutate(index=row_number(),regex=NA)
  for (i in 0:nregexes){
    errors <- errors %>% mutate(regex=replace(regex,
                                              is.na(regex) & index <= reader$f[['indptr']][i],
                                              i))
  }
  output <- list(errors,nfiles,nregexes)
  names(output) <- c('matrix','nfiles','nregexes')
  return(output)
}

## PCA generation
#
# Input: df: data frame containing
#   obs_cols: The column(s) that uniquely identify observations
#   feature_col: The column that uniquely identifies features
#   val_col: The column containing the values of features
#   dialect_col: The column that uniquely identifies dialects (in the training data)
#   nsample: number of points to produce (defaults to 10)
# Output:
#  Table with one row for each observation, and columns PC? for 
#  principal components
pca_generator <- function(df,obs_cols,feature_col,val_col,dialect_col,nsample=10){
  uniAsample_matrix <- df %>%
    filter(!is.na({{ obs_cols }})) %>% 
    filter({{ obs_cols }} %in% sample({{ obs_cols }},nsample)) %>%
    select({{ feature_col }},{{ obs_cols }},{{ val_col }}) %>%
    pivot_wider(names_from={{ feature_col }},
                id_cols={{ obs_cols }},
                values_from={{ val_col }},
                values_fill=0) %>%
    left_join(df %>% 
                filter(!is.na({{ obs_cols }})) %>% 
                group_by({{ obs_cols }}) %>% 
                summarize({{ dialect_col }}:=first({{ dialect_col }})),
              by=c('file'))
  
  pcaUniA<-uniAsample_matrix %>%
    select(-{{ obs_cols }},-{{ dialect_col }}) %>%
    prcomp()
  
  uniApca <- uniAsample_matrix %>% 
    bind_cols(pcaUniA$x%>%as_tibble())
}

## Bernoulli multi-way likelihood computation
#
# Input: 
#  test: The data for which likelihoods should be computed.  A table with at 
#       least two columns delineating observations and features
#  training: Training data for computing likelihoods.  A table with at 
#       least three columns delineating observations, features, and dialects
#  obs_cols: The column(s) that uniquely identify observations
#  feature_cols: The column(s) that uniquely identify features
#  dialect_cols: The column(s) that uniquely identify dialects (in the training data)
# Output: a tibble of likelihoods, each row corresponds to a file, and each 
#  column corresponds to a dialect
bernoulli_table <- function(test,training,obs_cols,feature_cols,dialect_cols){
  # Observation counts
  training_nobs <- training %>%
    group_by({{ dialect_cols }}) %>% 
    select({{ dialect_cols }},{{ obs_cols }}) %>% 
    summarize(obscount=n())

  # Compute feature-wise probabilities
  probs<-training %>%
    group_by({{ dialect_cols }}) %>%
    count({{ feature_cols }}) %>% 
    left_join(training_nobs, by=c(as_label(enquo(dialect_cols)))) %>%
    mutate(prob=n/obscount)
  
  # Extract observation IDs/numbers
  obslist <- test %>% select({{ obs_cols }}) %>% unique()
  
  obs_lrs <- NULL
  for(flnum in pull(obslist,{{ obs_cols }})){
    
    # Which features fire on this particular observation?
    matched_features <- test %>% 
      filter({{ obs_cols }} == flnum)

    # Which of these features should have fired, given it came from given set?
    correct_true <- probs %>%
      semi_join(matched_features, 
                c(as_label(enquo(dialect_cols)),
                  as_label(enquo(feature_cols)))) %>%
      group_by({{ dialect_cols }}) %>%
      summarize(ct=prod(prob),.groups='drop_last') 
    
    # Which of these regexes should not have fired, given it came from given set?
    correct_false <- probs %>%
      anti_join(matched_features, 
                c(as_label(enquo(dialect_cols)),
                  as_label(enquo(feature_cols)))) %>%
      group_by({{ dialect_cols }}) %>%
      summarize(cf=prod(1-prob),.groups='drop_last')
    
    # Aggregate results into likelihoods for each set
    vals <- correct_true %>% 
      full_join(correct_false,
                c(as_label(enquo(dialect_cols)))) %>%
      mutate(prob=if_else(is.na(ct*cf),0,ct*cf)) %>%
      select({{ dialect_cols }},prob) %>%
      pivot_wider(names_from={{ dialect_cols }},values_from=prob)%>%
      mutate({{ obs_cols }}:=flnum)
    
    # Store in table
    obs_lrs <- obs_lrs %>% bind_rows(vals)
  }
  
  return(obs_lrs)
}

## Generate weighted Dowker nested representation of the data
# Input: df = data frame to be consumed
#        feature_cols = the column in df for a tidy-select of variables 
#                       specifying features (this is the base space of )
#        obs_cols = the column in df for a tidy-select of variables 
#                   specifying the observations
# Output: a new data frame in which each row consists of several nested columns
#       feature_pattern = values from feature_cols specifying each unique pattern of 
#                 features found in df
#       observations = the observations that correspond to the feature pattern
#       weight = length of feature
#       nobs = length of observations
# 
# This function generates a minimal specification of the Dowker cosheaf 
# representation, in which the 
# * Base space vertices are the columns selected by feature_cols
# * The costalks (fibers) are geneated by the columns selected by obs_cols
# Note: The costalks for the Dowker cosheaf can be computed from these data by 
# taking the union of the observations associated with each feature pattern 
# that is less restrictive
dowker_nest <- function(df,feature_cols,obs_cols){
  df %>%
    ungroup() %>%
    select({{ feature_cols }},{{ obs_cols }}) %>%
    nest(feature_pattern={{ feature_cols }}) %>%
    group_by(feature_pattern) %>%
    nest(observations={{ obs_cols }}) %>%
    mutate(weight=sapply(observations,function(x){nrow(x)}),
           feature_count=sapply(feature_pattern,function(x){nrow(x)})) %>%
    ungroup()
}

## Compute Dowker posterior class probabilities from the output of dowker_nest
# 
# Input: df = output from dowker_nest
#        class_col = one of the column names in obs_col (used previously in 
#                    the call to dowker_nest) to identify classes of 
#                    observations
# Output: a new data frame with columns
#        feature_pattern = same as the input df
#        class_prob = nested column of tibbles containing two columns:
#              class_col = as before
#              prob      = posterior probability of class_col given 
#                          the feature_pattern
dowker_prob <- function(df,class_col){
  df %>% 
    unnest(observations) %>% 
    group_by(feature_pattern) %>% 
    count({{ class_col }}) %>% 
    mutate(feature_pattern,{{ class_col }},prob=n/sum(n),.keep='none') %>% 
    nest(class_prob=c({{ class_col }},prob)) %>% 
    ungroup()
}

# Construct graph from weighted Dowker nested data
# Input: a data frame in which each row consists of several nested columns
#       feature_pattern = values from feature_cols specifying each unique pattern of 
#                 features found in df
# Output: a new data frame with `source` and `destination` columns, both of 
#        which have values taken from the `feature_pattern` column of the input
# Note: this runs in O(n^2) where n is the number of rows in the input.  
#       Consider filtering by weight or nobs before using this function
dowker_graph <- function(dowker_table){
  with(dowker_table,
       cross_df(tibble(source=feature_pattern,
                       destination=feature_pattern),
                       .filter=function(x,y){(nrow(x)>=nrow(y)) || 
                           !any(nrow(anti_join(x,y,by=names(x)))==0)}))
}

# Compute the minimal elements from a graph, whose vertices are defined by a
# table `vertices` with a `node` column and edges defined by a table `grph` 
# with rows `source` and `destination`
# Other columns of `vertices` are preserved
minimal_elements <- function(grph,vertices){
  vertices %>% 
    anti_join(grph,by=c(node='destination'))
}

# Compute the maximum monotonic (non-strictly) decreasing function on a graph
# which is bounded above by an arbitrary function.
# The function is defined on vertices in a table `fcn` with a `node` column
# and `value` column.
# Edges of the graph are defined by a table `grph` 
# with rows `source` and `destination`
# `root_node` is a table with the same columns as `fcn` intended to be used
# to constrain the support of the function being constructed
# Note: this is only guaranteed to work if the graph is a directed acyclic graph
# Cycles in the graph will cause this function to crash
max_decreasing <- function(fcn,grph,root_nodes){
  current_stage <- root_nodes
  fcn_new <- root_nodes 
  
  while(nrow(current_stage)>0){ # Note: this condition is superfluous
    jnk <- current_stage %>%
      inner_join(grph,by=c(node='source')) %>% # Immediate successors of the minimal elements
      inner_join(fcn,by=c(destination='node')) %>%  # Collect function values already at successor
      select(-node) %>%
      pivot_longer(c(value.x,value.y),names_to='foo') %>%
      select(-foo) %>%  
      group_by(destination) 
    
    if(nrow(jnk) == 0 ) {break;} # Check to see if we're finished
    
    current_stage <- jnk %>%
      summarize(value=min(value)) %>% # Maximum permissible values on immediate successors
      mutate(node=destination,value=value,.keep='none') # Fix names
    
    # Merge in the new results.  
    # Note that we might incur some duplicates if the graph is not a Hasse 
    # diagram in that it has "short cut" paths
    fcn_new <- fcn_new %>% 
      bind_rows(current_stage) %>%
      group_by(node) %>%
      summarize(value=min(value))
  }
  return(fcn_new)
}

# Compute a decomposition of an arbitrary function on the graph into a sum of
# maximum monotonic (non-strictly) decreasing functions.
#
# Input: 
# `fcn` : The function is defined on vertices in a table as follows:
#   The vertices are defined by the column identified by `node_col`
#   The values are in the column identified by `value_col`
#  `grph` : The edges of the graph, a table 
#    with columns `source` and `destination`
# Output: Rebuilt `fcn` table with a new column and new rows
#   `decomp` listing the new functions with values in `weight`
#
# Hint: you can pipeline the output of this function into 
#  `pivot_wider(node_col,names_from=decomp,values_from=value_col)`
# (replacing `node_col` and `value_col` with the appropriate column
# names) to recover the original rows of `fcn` with the new functions as 
# additional columns.
#
# This is only guaranteed to work if the graph is a directed acyclic graph
# Caution: Cycles in the graph will cause this function to crash
#
# Caution: Do not name columns of `fcn` either `node` or `value`
# as these are used internally
max_decreasing_decomp <- function(fcn,grph,node_col,value_col){

  # For metaprogramming sanity, defuse the names
  fcn_defused <- fcn %>%
  mutate(node={{ node_col }},
         value={{ value_col }}) %>%
    select(-{{ node_col }}, -{{ value_col }})
    
  # Initial setup
  i <- 1
  grph_new <- grph
    
  fcn_remaining <- fcn_defused # This will be the remaining portion of the function left to decompose
  fcn_new <- fcn_defused %>% 
    mutate(decomp='original')
  
  while(any(fcn_remaining$value>0)){
    # Determine the next remaining graph minimal element
    minimal <- minimal_elements(grph_new, fcn_remaining) %>% 
      slice_max(value,n=1) %>% 
      slice_head()
    
    # Compute the maximum decreasing function bounded by what remains
    md <- fcn_remaining %>% 
      max_decreasing(grph_new,minimal)
    
    # Splice in zeros for the values outside the support of the current minimal element
    md <- md %>% 
      bind_rows(fcn_defused %>% 
                  select(node) %>% 
                  anti_join(md,by=c(node='node')) %>%
                  mutate(value=0L))
    
    # Record maximum decreasing function
    fcn_new <- fcn_new %>% 
      bind_rows(minimal %>% 
                  filter(value>0) %>%
                  mutate(decomp='root_count')) %>%
      bind_rows(md %>% 
                  mutate(decomp=as.character(i)))
    
    # Determine the amount of function left to decompose after this is complete
    fcn_remaining <- fcn_new %>% 
      filter(decomp!='original',decomp!='root_count') %>% 
      group_by(node) %>% 
      summarize(value=sum(value)) %>%
      left_join(fcn_defused,by=c(node='node')) %>%
      mutate(value=value.y-value.x) %>%
      select(node,value)
    
    # Prune graph by removing edges from minimal element and any that have been
    # zeroed out by removal above
    grph_new <- grph_new %>% 
      anti_join(minimal, by=c(source='node')) %>%
      anti_join(fcn_remaining %>% filter(value==0), by=c(source='node'))
    
    i <- i+1
  }
  
  # Recover old column names
  fcn_new %>% 
    mutate({{ node_col }}:=node,
           {{ value_col }}:=value) %>%
    select(-node,-value) %>%
    return()
}
