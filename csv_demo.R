# Dowker tool demonstration script: CSV files
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
# License for the specific language governing permissions and limitations under
# the License.
#
# Special thanks to: Tate Altman
#
# Data credit: 
# G. J. J. van den Burg, A. Nazábal, and C. Sutton, "Wrangling messy CSV files 
# by detecting row and type patterns," Data Mining and Knowledge Discovery,
# vol. 33, no. 6, pp. 1799–1820, 2019. [https://doi.org/10.1007/s10618-019-00646-y]
#
# This material is based upon work supported by the Defense Advanced Research 
# Projects Agency (DARPA) SafeDocs program under contract HR001119C0072. 
# Any opinions, findings and conclusions or recommendations expressed in 
# this material are those of the authors and do not necessarily reflect the 
# views of DARPA.

library(tidyverse)
library(igraph)

source('dowker_data_tools.R')

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
