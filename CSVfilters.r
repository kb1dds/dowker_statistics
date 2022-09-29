library(tidyverse)
library(dplyr)
library(tidyr)
np<-import('numpy')

#Filters_encoding contains the additional encoding feature

filters_encoding <- read_csv("/Users/tatealtman/Desktop/CSVfilters.csv")

#this step removes the encoding feature

filters <- filters_encoding[-(5)]

dowker_cosheaf <- function(df,left_var,right_var){
  df %>%
    ungroup() %>%
    select({{left_var}},{{right_var}}) %>%
    nest(simplex={{left_var}}) %>%
    group_by(simplex) %>%
    nest(costalk={{right_var}}) %>%
    mutate(weight=sapply(costalk,function(x){nrow(x)}),
           dimension=sapply(simplex,function(x){nrow(x)-1})) %>%
    ungroup()
}

dowkercosheaf_filters<- filters%>%
  dowker_cosheaf(c(DELIMITER,ESCAPECHAR,QUOTECHAR),FILEHASH)
  
weight_filters <- dowkercosheaf_filters%>%select(c(simplex,weight))

weight_filters_unnest <- weight_filters%>%unnest(simplex)

weight_filters_unnest_df <- as.data.frame(weight_filters_unnest)

weight_filters_unnest_desc <- weight_filters_unnest%>%arrange(desc(weight))

weight_filters_unnest_desc %>% print(n=28)

