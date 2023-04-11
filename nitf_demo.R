# Dowker tool demonstration script
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

library(tidyverse)
library(reticulate)
library(jsonlite)
library(plotly)
np<-import('numpy') # Needed to parse the npz files from BAE

# Bring in the tools!
source('~/robinson-hackathon-6/eval_data_tools.R')

# Load the data

## BAE regexes
nitf_bae_names <- read_table('~/robinson-hackathon-6/nitf-filelist.txt',col_names = 'filenm') %>%
  mutate(file=row_number())

nitf_raw_bae <- npz_error_matrix_reader('robinson-hackathon-6/error_matrix2.npz')
nitf_bae <- nitf_raw_bae$matrix %>% mutate(regex=as.factor(regex)) %>%
  left_join(nitf_bae_names,by=c(file='file')) %>%
  select(-file)  # We are going to replace these indices by the crashes one below; it is more complete

json <- fromJSON('~/robinson-hackathon-6/ontology.json')
regex_ontology <- tibble(regex=names(json),
                         value=json) %>% 
  mutate(regex=as.factor(as.integer(regex))) %>%
  unnest_longer(value,
                indices_to='key') %>%
  pivot_wider(id_cols='regex',names_from='key',values_from='value') %>%
  mutate(parser=as.character(parser) %>% as.factor()) %>%
  select(-taxonomy)

## NITF file names
nitf_crashes_names <- read_delim('~/robinson-hackathon-6/nitf.list',delim=',',col_names='filename') %>%
  mutate(subcorpus=map(filename,~str_split(.,'/')%>%
                         unlist()%>%
                         pluck(5))%>%
           unlist(),
         filenm=map(filename,~str_split(.,'/')%>%
                      unlist()%>%
                      last()%>%
                      str_split('[.]')%>%
                      unlist()%>%
                      first())%>%
           unlist(),
         file=row_number())

## Crash log + GDAL
nitf_crashes <- read_delim('~/robinson-hackathon-6/crash.out',col_names=c('parser','jnk','filename')) %>%
  mutate(subcorpus=map(filename,~str_split(.,'/')%>%
                         unlist()%>%
                         pluck(5))%>%
           unlist(),
         filenm=map(filename,~str_split(.,'/')%>%
                        unlist()%>%
                        last()%>%
                      str_split('[.]')%>%
                      unlist()%>%
                      first())%>%
           unlist()
  ) %>% 
  left_join(nitf_crashes_names, by=c(filenm='filenm')) %>%
  mutate(subcorpus=subcorpus.x,
         filename=filename.x) %>%
  select(-subcorpus.x,-subcorpus.y,-filename.x,-filename.y)

### Add subcorpus information
subcorpora <- nitf_crashes_names$subcorpus %>% unique()

nitf_bae <- nitf_bae %>% 
  left_join(nitf_crashes_names, by=c(filenm='filenm'))

# Quicklook visualization to make sure the data arrived correctly
nitf_bae %>% 
  ggplot(aes(x=as.integer(as.factor(file)),y=as.integer(regex))) + 
  geom_bin2d(bins=c(100,1000),show.legend = FALSE) + 
  ylab('Message') + xlab('File')

nitf_crashes %>% ggplot(aes(x=file,y=parser)) + 
  geom_bin2d(bins=c(100,1000),show.legend=FALSE) +
  ylab('Message') + xlab('File')

# PCA plots

## Crashes
pca_crashes <- nitf_crashes %>% 
  mutate(error=1) %>%
  pca_generator(file,parser,error,subcorpus,1000)

pca_crashes %>% 
  plot_ly(x=~PC1,y=~PC2,z=~PC3,text=~file) %>% 
  add_markers(color=~subcorpus,size=2)

## BAE regexes
pca_bae <- nitf_bae %>% 
  pca_generator(file,regex,error,subcorpus,1000)

pca_bae %>% 
  plot_ly(x=~PC1,y=~PC2,z=~PC3,text=~file) %>% 
  add_markers(color=~subcorpus,size=2)

# Source-source distances comparing crashes to BAE data
reldist<-import('relations.kappa')

reldists <- tibble(subcorpus=c())

for(subc in subcorpora){
  # Subset the data
  set1 <- nitf_bae %>% filter(subcorpus == subc)
  set2 <- nitf_crashes %>% filter(subcorpus == subc)
    
  # Ensure the same files are triggered in both
  # (this ensures that the relation matrices involved have no blank rows.  
  # This is a requirement for the distance algorithm)
  filelist <- intersect(pull(set1,file),pull(set2,file))
    
  if(length(filelist) > 0){
    # Build relation matrices
    set1sample <- set1 %>% 
      filter( file %in% filelist ) %>%
      pivot_wider(names_from=regex,id_cols=file,values_from=error,values_fill=0) %>%
      select(-file) %>%
      as.matrix()
    
    set2sample <- set2 %>% 
      mutate(error=1)%>%
      filter( file %in% filelist ) %>%
      pivot_wider(names_from=parser,id_cols=file,values_from=error,values_fill=0) %>%
      select(-file) %>%
      as.matrix()
    
    # Compute distance
    reldists <- reldists %>% 
      bind_rows(tibble(subcorpus=subc,
                       distance=reldist$rel_dist_bound(set1sample,set2sample),
                       relative_distance=distance/length(filelist)))
  }
}

## Display subcorpus distances
reldists %>% ggplot(aes(subcorpus,relative_distance)) +
  geom_col()

# Bernoulli misclassification test

## Crashes
crashes_bern <- nitf_crashes %>%
  bernoulli_table(nitf_crashes,file,parser,subcorpus)

### Performance check
crashes_bern %>% 
  pivot_longer(cols=!file,names_to='subcorpus',values_to='prob') %>%
  group_by(file) %>%
  slice_max(prob) %>%
  slice_head(n=1) %>%
  left_join(nitf_crashes_names, by=c('file')) %>%
  mutate(correct=(subcorpus.x==subcorpus.y)) %>%
  ungroup() %>%
  group_by(subcorpus.x) %>%
  count(correct) %>%
  ungroup() %>%
  pivot_wider(id_cols=subcorpus.x,names_from=correct,values_from=n,values_fill=0) %>%
  mutate(subcorpus=subcorpus.x,score=(`TRUE`/(`TRUE`+`FALSE`)),.keep='none')

## BAE regexes
bae_bern <- nitf_bae %>%
  bernoulli_table(nitf_bae,file,regex,subcorpus)

### Performance check
bae_bern %>% 
  pivot_longer(cols=!file,names_to='subcorpus',values_to='prob') %>%
  group_by(file) %>%
  slice_max(prob) %>%
  slice_head(n=1) %>%
  left_join(nitf_crashes_names, by=c('file')) %>%
  mutate(correct=(subcorpus.x==subcorpus.y)) %>%
  ungroup() %>%
  group_by(subcorpus.x) %>%
  count(correct)%>%
  ungroup() %>%
  pivot_wider(id_cols=subcorpus.x,names_from=correct,values_from=n,values_fill=0) %>%
  mutate(subcorpus=subcorpus.x,score=(`TRUE`/(`TRUE`+`FALSE`)),.keep='none')

# Construct Dowker tables
bae_dowker_table <- nitf_bae %>% 
  dowker_nest(feature_cols=regex,
              obs_cols=c(file,subcorpus))

crash_dowker_table <- nitf_crashes %>% 
  dowker_nest(feature_cols = parser,
              obs_cols = c(file,subcorpus))

# Dowker histograms
bae_dowker_table %>%
  mutate(idx=row_number()) %>%
  ggplot(aes(x=idx,y=sort(weight,decreasing = TRUE))) +
  geom_line() + 
  scale_y_log10() +
  xlab('Message pattern') + ylab('File count')

crash_dowker_table %>%
  mutate(idx=row_number()) %>%
  ggplot(aes(x=idx,y=sort(weight,decreasing = TRUE))) +
  geom_line() + 
  scale_y_log10() +
  xlab('Crash pattern') + ylab('File count')

# Dowker tables by source

## Crashes
crash_breakdown <- crash_dowker_table %>%
  mutate(subcorpus_counts=map(observations,~count(.,subcorpus))) %>%
  unnest(subcorpus_counts) %>%
  pivot_wider(id_cols=feature_pattern,
              names_from=subcorpus,
              values_from=n,
              values_fill=0)

crash_breakdown %>%
  mutate(feature_pattern=map(feature_pattern,~str_flatten(.$parser,collapse=' '))%>%unlist)

## BAE regexes
bae_breakdown <- bae_dowker_table %>%
  mutate(subcorpus_counts=map(observations,~count(.,subcorpus))) %>%
  unnest(subcorpus_counts) %>%
  pivot_wider(id_cols=feature_pattern,
              names_from=subcorpus,
              values_from=n,
              values_fill=0)

bae_breakdown %>%
  mutate(feature_pattern=map(feature_pattern,~str_flatten(.$regex,collapse=' '))%>%unlist) %>%
  print(n=20)

# Posterior probability tables

## Crashes
crash_dowker_detector <- crash_dowker_table %>%
  dowker_prob(subcorpus)

crash_dowker_detector %>% 
  unnest(class_prob) %>% 
  pivot_wider(id_cols=feature_pattern,
              names_from=subcorpus,
              values_from=prob,values_fill=0) %>%
  mutate(feature_pattern=map(feature_pattern,~str_flatten(.$parser,collapse=' '))%>%unlist)

## BAE regexes
bae_dowker_detector <- bae_dowker_table %>%
  dowker_prob(subcorpus)

bae_dowker_detector %>% 
  unnest(class_prob) %>% 
  pivot_wider(id_cols=feature_pattern,
              names_from=subcorpus,
              values_from=prob,values_fill=0) %>%
  mutate(feature_pattern=map(feature_pattern,~str_flatten(.$regex,collapse=' '))%>%unlist)

# Building dialect partitions

## Crashes
crashes_graph <- dowker_graph(crash_dowker_table)

crashes_dialects <- crash_dowker_table %>% 
  max_decreasing_decomp(crashes_graph,feature_pattern,weight)  %>% 
  pivot_wider(feature_pattern,
              names_from=decomp,
              values_from=weight)

# Summary table
crashes_dialects_summary <- crashes_dialects %>%
  left_join(crash_breakdown,by=c(feature_pattern='feature_pattern')) %>%
  mutate(feature_pattern=map(feature_pattern,
                             ~str_flatten(.$parser,collapse=' '))%>%unlist)

crashes_dialects_summary

## BAE regexes
bae_graph <- dowker_graph(bae_dowker_table)

bae_dialects <- bae_dowker_table %>% 
  max_decreasing_decomp(bae_graph,feature_pattern,weight) %>% 
  pivot_wider(feature_pattern,
              names_from=decomp,
              values_from=weight)

bae_dialects_summary <- bae_dialects %>%
  left_join(bae_breakdown,by=c(feature_pattern='feature_pattern')) %>%
  mutate(feature_pattern=map(feature_pattern,
                             ~str_flatten(.$regex,collapse=' '))%>%unlist) %>%
  arrange(desc(original)) %>% print(n=20)

# Probability of a file matching a source given detected dialect

## Crashes

crashes_cluster_summary <- crashes_dialects_summary %>% 
  select(-original,-root_count) %>% 
  pivot_longer(cols=-feature_pattern,names_to='cluster',values_to='n') 

crashes_clusters <- crashes_dialects %>% 
  slice_head() %>% 
  select(-original,-root_count) %>% 
  pivot_longer(cols=-feature_pattern,names_to='cluster',values_to='n')
crashes_clusters <- crashes_clusters$cluster

crashes_cluster_pairwise <- expand_grid(item1=unique(crashes_cluster_summary$cluster),
            item2=unique(crashes_cluster_summary$cluster)) %>%
  right_join(crashes_cluster_summary,by=c(item1='cluster')) %>%
  right_join(crashes_cluster_summary,by=c(item2='cluster',feature_pattern='feature_pattern')) %>%
  left_join(crashes_cluster_summary %>% group_by(feature_pattern) %>% summarize(total=sum(n)),
            by=c(feature_pattern='feature_pattern')) 

### Probability of one file matching
crashes_cluster_pairwise %>%
  mutate(p_one_hit=1-dhyper(0,n.y,total,n.x),
         wt=total*p_one_hit)  %>%
  group_by(item1,item2) %>%
  summarize(p=sum(wt)/sum(total)) %>%
  filter(item1 %in% crashes_clusters,
         ! item2 %in% crashes_clusters ) %>%
  ggplot(aes(item1,item2,fill=p)) + geom_raster() +
  xlab('Detected dialect') + ylab('File source')

## BAE regexes

bae_cluster_summary <- bae_dialects_summary %>% 
  select(-original,-root_count) %>% 
  pivot_longer(cols=-feature_pattern,names_to='cluster',values_to='n') 

bae_clusters <- bae_dialects %>% 
  slice_head() %>% 
  select(-original,-root_count) %>% 
  pivot_longer(cols=-feature_pattern,names_to='cluster',values_to='n')
bae_clusters <- bae_clusters$cluster

bae_cluster_pairwise <- expand_grid(item1=unique(bae_cluster_summary$cluster),
                                        item2=unique(bae_cluster_summary$cluster)) %>%
  right_join(bae_cluster_summary,by=c(item1='cluster')) %>%
  right_join(bae_cluster_summary,by=c(item2='cluster',feature_pattern='feature_pattern')) %>%
  left_join(bae_cluster_summary %>% group_by(feature_pattern) %>% summarize(total=sum(n)),
            by=c(feature_pattern='feature_pattern')) 

### Probability of one file matching
bae_cluster_pairwise %>%
  mutate(p_one_hit=1-dhyper(0,n.y,total,n.x),
         wt=total*p_one_hit)  %>%
  group_by(item1,item2) %>%
  summarize(p=sum(wt)/sum(total)) %>%
  filter(item1 %in% bae_clusters,
         ! item2 %in% bae_clusters ) %>%
  ggplot(aes(item2,item1,fill=p)) + geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) + 
  ylab('Detected dialect') + xlab('File source')

