# Dowker splitting algorithm implementation

library(tidyverse)

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
  mutate(test1=(f1>=f1.1),
         test2=(f2>=f2.1),
         test3=(f3>=f3.1))

fcn %>% 
  anti_join(grph,by=c(node='destination'))