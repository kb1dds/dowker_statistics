# Dowker splitting algorithm implementation

library(tidyverse)
library(igraph)

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

# Sample graph data
grph <- tibble(source=c('A','B','B','B','C','C','D','D','BC','BD','CD'),
               destination=c('AB','AB','BC','BD','BC','CD','BD','CD','BCD','BCD','BCD'))

# Sample function data
fcn <- tibble(vertex=c('A','B','C','D','E','AB','BC','BD','CD','BCD'),
              weight=c(3,5,2,4,8, 4,7,1,6, 2))

# A correct decomposition
fcn_decomp <- fcn %>% 
  mutate(f1=c(3,5,2,4,8, 3,2,1,2, 1),
         f2=c(0,0,0,0,0, 1,5,0,4, 0),
         f3=c(0,0,0,0,0, 0,0,0,0, 1))

# Verify the decomposition for sum: This should be TRUE
fcn_decomp %>% 
  mutate(test=((weight-f1-f2-f3)==0)) %>%
  summarize(all(test))

# Verify the decomposition for ordering
grph %>%
  left_join(fcn_decomp,by=c(source='vertex')) %>%
  left_join(fcn_decomp,by=c(destination='vertex')) %>%
  mutate(test1=(f1.x>=f1.y),
         test2=(f2.x>=f2.y),
         test3=(f3.x>=f3.y))

# Decompose function
max_decreasing_decomp(fcn,grph,vertex,weight) %>% 
  pivot_wider(vertex,names_from=decomp,values_from=weight)

##### A different example: 
##### byte statistics in ROM containing executable binary code for three 
##### different kinds of CPUs

data <- read_csv('uc07_rom_windows.csv') %>%
  pivot_longer(cols=!byte_offset,
               names_to='byte_value',
               values_to='count') %>%
  filter(count>0)

dowker_table <- data %>% 
  filter(count>40)%>% # Determined by manual experimentation
  dowker_nest(feature_cols = byte_value,obs_cols = byte_offset)

dg <- dowker_graph(dowker_table)

dd <- max_decreasing_decomp(dowker_table, 
                      dg,
                      feature_pattern, 
                      weight) %>% 
  pivot_wider(feature_pattern,names_from=decomp,values_from=weight)

# Modification of the above example; instead of filtering for large byte counts directly,
# aggregate them randomly first

agg_table <- tibble(byte_value=0:255 %>% as.character, 
                    group=sample.int(10,size=256,replace=TRUE))

dowker_table2 <- data %>% 
  left_join(agg_table, by=c(byte_value='byte_value')) %>%
  group_by(byte_offset,group) %>%
  summarize(count=sum(count)) %>%
  filter(count > 50) %>%
  dowker_nest(feature_cols = group,obs_cols = byte_offset)

dg2 <- dowker_graph(dowker_table2)

dd2 <- max_decreasing_decomp(dowker_table2,
                             dg2,
                             feature_pattern,
                             weight) %>% 
  pivot_wider(feature_pattern,names_from=decomp,values_from=weight)

##### CSV data example

data_csv <- read_csv('CSVfilters.csv')

data_csv_rel <- data_csv %>% 
  mutate(ENCODING=str_to_upper(str_conv(ENCODING,'UTF-8'))) %>%  # Ha.  Really.
  pivot_longer(!FILEHASH,names_to='feature_type',values_to='feature') %>%
  filter(feature!='n/a')

data_csv_rel %>% 
  mutate(message=paste(feature_type,feature)) %>%
  ggplot(aes(x=FILEHASH,y=message)) + 
  geom_bin_2d()

csv_dowker_table <- data_csv_rel %>%
    dowker_nest(feature_cols=feature,
                obs_cols = FILEHASH)

csv_dg <- dowker_graph(csv_dowker_table)

csv_dd <- max_decreasing_decomp(csv_dowker_table,
                                csv_dg,
                                feature_pattern,
                                weight) %>% 
  pivot_wider(feature_pattern,names_from=decomp,values_from=weight)

csv_dd %>% 
  mutate(feature_pattern=map(feature_pattern,
                             ~str_flatten(.$feature,collapse=' '))%>%unlist) %>%
  select(-feature_pattern) %>%
  write_csv('CSVfilters_decomp.csv')

# igraph drawing

## This is a bit annoying because igraph wants integer vertex IDs, whereas it's 
## more natural to use R lists instead.  We therefore need to do a bit of 
## translation in order to construct the graph.

csv_dt_ind <- csv_dowker_table %>% 
  mutate(idx=row_number())

csv_dg_ind <- csv_dg %>% 
  left_join(csv_dt_ind %>% select(-weight), by=c(source='feature_pattern')) %>%
  transmute(destination=destination,
            source=idx) %>%
  left_join(csv_dt_ind %>% select(-weight), by=c(destination='feature_pattern')) %>%
  transmute(source=source,
            destination=idx)

csv_dg_ig <- make_empty_graph() +
  vertices(csv_dt_ind$idx,
           label=map(csv_dowker_table$feature_pattern,
                     ~str_flatten(.$feature,collapse=' ')),
           size=3*log10(csv_dowker_table$weight)) +
  graph_from_data_frame(csv_dg_ind)

## Expect to do some manual adjustment...
tkplot(csv_dg_ig)
