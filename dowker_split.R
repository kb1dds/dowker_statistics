# Dowker splitting algorithm implementation

library(tidyverse)

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
# Note: this is only guaranteed to work if the graph is a directed acyclic graph
# Cycles in the graph will cause this function to crash
max_decreasing <- function(grph,fcn){
  current_stage <- minimal_elements(grph,fcn) # These are the minimal elements
  
  fcn_new <- current_stage
  
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
      transmute(node=destination,value=value) # Fix names
    
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
# The function is defined on vertices in a table `fcn` with a `nodes` column
# and `value` column.
# Edges of the graph are defined by a table `grph` 
# with rows `source` and `destination`
# Note: this is only guaranteed to work if the graph is a directed acyclic graph
# Cycles in the graph will cause this function to crash
max_decreasing_decomp <- function(grph,fcn){
  
  # Initial setup
  i <- 1
  grph_new <- grph
  fcn_remaining <- fcn # This will be the remaining portion of the function left to decompose
  fcn_new <- fcn %>% 
    mutate(decomp='original')
  
  while(any(fcn_remaining$value>0)){
    # Determine the remaining graph minimal elements
    minimal <- minimal_elements(grph_new, fcn_remaining)
  
    # Compute the maximum decreasing function bounded by what remains
    fcn_new <- fcn_new %>% 
      bind_rows(max_decreasing(grph_new,fcn_remaining) %>% 
                  mutate(decomp=as.character(i)))
    
    # Determine the amount of function left to decompose after this is complete
    fcn_remaining <- fcn_new %>% 
      filter(decomp!='original') %>% 
      group_by(node) %>% 
      summarize(value=sum(value)) %>%
      left_join(fcn,by=c(node='node')) %>%
      mutate(value=value.y-value.x) %>%
      select(node,value)
    
    # Prune graph by removing edges from minimal elements
    grph_new <- grph_new %>% 
      anti_join(minimal, by=c(source='node'))
    
    i <- i+1
  }

  return(fcn_new)
}

## Generate weighted Dowker nested representation of the data
# Input: df = data frame to be consumed
#        feature_vars = the column in df for a tidy-select of variables 
#                       specifying features (this is the base space of )
#        obs_vars = the column in df for a tidy-select of variables 
#                   specifying the observations
# Output: a new data frame in which each row consists of several nested columns
#       feature_pattern = values from left_var specifying each unique pattern of 
#                 features found in df
#       observations = the observations that correspond to the feature pattern
#       weight = length of feature
#       nobs = length of observations
# 
# This function generates a minimal specification of the Dowker cosheaf 
# representation, in which the 
# * Base space vertices are the columns selected by feature_vars
# * The costalks (fibers) are geneated by the columns selected by obs_vars
# Note: The costalks for the Dowker cosheaf can be computed from these data by 
# taking the union of the observations associated with each feature pattern 
# that is less restrictive
dowker_nest <- function(df,feature_vars,obs_vars){
  df %>%
    ungroup() %>%
    select({{feature_vars}},{{obs_vars}}) %>%
    nest(feature_pattern={{feature_vars}}) %>%
    group_by(feature_pattern) %>%
    nest(observations={{obs_vars}}) %>%
    mutate(weight=sapply(observations,function(x){nrow(x)}),
           feature_count=sapply(feature_pattern,function(x){nrow(x)})) %>%
    ungroup()
}

# Construct graph from weighted Dowker nested data
# Input: a data frame in which each row consists of several nested columns
#       feature_pattern = values from left_var specifying each unique pattern of 
#                 features found in df
# Output: a new data frame with `source` and `destination` columns, both of 
#        which have values taken from the `feature_pattern` column of the input
# Note: this runs in O(n^2) where n is the number of rows in the input.  
#       Consider filtering by weight or nobs before using this function
dowker_graph <- function(dowker_table){
  with(dowker_table,
       cross_df(tibble(source=feature_pattern,
                       destination=feature_pattern),
                       .filter=function(x,y){(nrow(x)>=nrow(y)) |
                           any(nrow(anti_join(x,y,by=NULL)==0))}))
}

# Sample graph data
grph <- tibble(source=c('A','B','B','B','C','C','D','D','BC','BD','CD'),
               destination=c('AB','AB','BC','BD','BC','CD','BD','CD','BCD','BCD','BCD'))

# Sample function data
fcn <- tibble(node=c('A','B','C','D','E','AB','BC','BD','CD','BCD'),
              value=c(3,5,2,4,8, 4,7,1,6, 2))

# A correct decomposition
fcn_decomp <- fcn %>% 
  mutate(f1=c(3,5,2,4,8, 3,2,1,2, 1),
         f2=c(0,0,0,0,0, 1,5,0,4, 0),
         f3=c(0,0,0,0,0, 0,0,0,0, 1))

# Verify the decomposition for sum: This should be TRUE
fcn_decomp %>% 
  mutate(test=((value-f1-f2-f3)==0)) %>%
  summarize(all(test))

# Verify the decomposition for ordering
grph %>%
  left_join(fcn_decomp,by=c(source='node')) %>%
  left_join(fcn_decomp,by=c(destination='node')) %>%
  mutate(test1=(f1.x>=f1.y),
         test2=(f2.x>=f2.y),
         test3=(f3.x>=f3.y))

# Decompose function
max_decreasing_decomp(grph,fcn) %>% pivot_wider(node,names_from=decomp,values_from=value)

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
  dowker_nest(feature_vars = byte_value,obs_vars = byte_offset)

dg <- dowker_graph(dowker_table)

dd <- max_decreasing_decomp(dg,
                      dowker_table %>% transmute(node=feature_pattern,
                                                 value=weight)) %>% 
  pivot_wider(node,names_from=decomp,values_from=value)

# Modification of the above example; instead of filtering for large byte counts directly,
# aggregate them randomly first

agg_table <- tibble(byte_value=0:255 %>% as.character, 
                    group=sample.int(10,size=256,replace=TRUE))

dowker_table2 <- data %>% 
  left_join(agg_table, by=c(byte_value='byte_value')) %>%
  group_by(byte_offset,group) %>%
  summarize(count=sum(count)) %>%
  filter(count > 50) %>%
  dowker_nest(feature_vars = group,obs_vars = byte_offset)

dg2 <- dowker_graph(dowker_table2)

dt2 <- dowker_table2 %>% transmute(node=feature_pattern,value=weight)

dd2 <- max_decreasing_decomp(dg2,dt2) %>% 
  pivot_wider(node,names_from=decomp,values_from=value)

##### CSV data example

data_csv <- read_csv('CSVfilters.csv')

data_csv_rel <- data_csv %>% 
  pivot_longer(!FILEHASH,names_to='feature_type',values_to='feature') %>%
  filter(feature!='n/a')

csv_dowker_table <- data_csv_rel %>%
    dowker_nest(feature_vars=feature,
                obs_vars = FILEHASH)

csv_dg <- dowker_graph(csv_dowker_table)

csv_dt <- csv_dowker_table %>% transmute(node=feature_pattern,value=weight)

csv_dd <- max_decreasing_decomp(csv_dg,csv_dt) %>% 
  pivot_wider(node,names_from=decomp,values_from=value)

csv_dd %>% 
  mutate(feature_pattern=map(node,~str_flatten(.$feature,collapse=' '))%>%unlist) %>%
  select(-node) %>%
  write_csv('CSVfilters_decomp.csv')
