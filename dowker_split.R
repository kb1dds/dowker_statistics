# Dowker splitting algorithm implementation

library(tidyverse)

# Compute the minimal elements from a graph, whose vertices are defined by a
# table `vertices` with a `nodes` column and edges defined by a table `grph` 
# with rows `source` and `destination`
# Other columns of `vertices` are preserved
minimal_elements <- function(grph,vertices){
  vertices %>% 
    anti_join(grph,by=c(node='destination'))
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

  fcn_new <- fcn_new %>% bind_rows(current_stage)
}
