# Dependencies 
# Please download gutenbergr, tidyverse, tm, tidytext, ggthemes, gridExtra to use this package 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0-  Loading Libraries                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pacman)
## Loading required packages and install them if not yet installed 
p_load(gutenbergr, tidyverse, tm, tidytext, ggthemes, gridExtra)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.1 -  Loading data                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Use Gutenberg book data
gutenberg_metadata <- gutenberg_metadata
read.csv("gutenberg_sample_data.csv", header = TRUE, sep = ",") -> id_topics
gutenberg_15books <- gutenberg_download(c(id_topics$gutenberg_id), mirror = "http://mirrors.xmission.com/gutenberg/")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2-  tf-idf analysis                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# tf-idf analysis
book_words <- gutenberg_15books %>%  
  unnest_tokens(word, text) %>%  
  count(gutenberg_id, word, sort = TRUE) %>%  
  ungroup()

total_words <- book_words %>%  
  group_by(gutenberg_id) %>%  
  summarize(total = sum(n))

# create new stop word list using tf_idf 
left_join(book_words, total_words) %>%  
  bind_tf_idf(word, gutenberg_id, n) -> book_words

book_words%>%  
  filter(tf_idf < 0.001) %>%  
  select(word) %>%  
  unique() -> stop_words_tf_idf

stop_words <- removeNumbers(stop_words_tf_idf$word) # remove numbers 

# 
book_words <- left_join(gutenberg_15books, id_topics, by = "gutenberg_id") %>%  
  unnest_tokens(word, text) %>%  
  count(Topics, gutenberg_id, word, sort = TRUE) %>%  
  ungroup()

total_words <- book_words %>%  
  group_by(Topics, gutenberg_id) %>%  
  summarize(total = sum(n))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4-  gutenberg_id and text                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

left_join(gutenberg_15books, id_topics, by = "gutenberg_id") %>% 
  group_by(gutenberg_id, Topics) %>% 
  slice_sample(n=500, replace = TRUE) %>% 
  unnest_tokens(word, text) -> test


test %>%
  anti_join(stop_words_tf_idf, by = "word") %>%
  unique() %>% 
  ungroup() %>% 
  add_count(word) %>% 
  slice_max(n, n = 100000) %>% 
  select(-c("n")) -> test_1

# antijoin, tidytext stop words 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8-  Dowker Complex Nest Function by Dr. Michael Robinson                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define the dowker_nest function 
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

#### Join the topics to the books_nonzero data
test_1 %>%  
  rename(Docs = gutenberg_id, 
         Terms = word) -> test_2

dowker_nest(, Terms, c(Docs, Topics)) -> books_dowker_nest


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9-  Add columns (Comment later, verification)                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# add count columns 
for (i in 1:nrow(books_dowker_nest)) {
  books_dowker_nest$politics_count[i] <- sum(books_dowker_nest[[1]][[i]] < 21) 
  books_dowker_nest$art_count[i] <- sum( 20 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 41) 
  books_dowker_nest$biology_count[i] <- sum( 40 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 61) 
  books_dowker_nest$cookery_count[i] <- sum( 60 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 81) 
  books_dowker_nest$travel_count[i] <- books_dowker_nest[[4]][[i]] - sum(books_dowker_nest[[1]][[i]] < 21) - sum( 20 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 41)-  sum( 40 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 61)- sum( 60 < books_dowker_nest[[1]][[i]] & books_dowker_nest[[1]][[i]]< 81)  
}

# Add max column 
for (i in 1:nrow(books_dowker_nest)) {
  books_dowker_nest$max[i] <- max(books_dowker_nest[[5]][[i]], 
                                  books_dowker_nest[[6]][[i]], 
                                  books_dowker_nest[[7]][[i]], 
                                  books_dowker_nest[[8]][[i]], 
                                  books_dowker_nest[[9]][[i]])
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10-  Median                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Create Median function 
mymedian <- function(l) {
  n <- length(l)
  s <- sort(l)
  ifelse(n%%2==1,s[(n+1)/2],mean(s[n/2+0:1]))
}

# Add median column 
for (i in 1:nrow(books_dowker_nest)) {
  books_dowker_nest$median[i] <- mymedian(c(books_dowker_nest[[5]][[i]], 
                                            books_dowker_nest[[6]][[i]], 
                                            books_dowker_nest[[7]][[i]], 
                                            books_dowker_nest[[8]][[i]], 
                                            books_dowker_nest[[9]][[i]]))
}

# Add difference column 
books_dowker_nest %>%  
  mutate(difference = max - median) -> books_dowker_nest

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 11-  Probability                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# probability of each topic by full count
books_dowker_nest %>%  
  mutate(prob_art = art_count/feature_count) %>%  
  mutate(prob_biology = biology_count/feature_count)  %>%  
  mutate(prob_politics = politics_count/feature_count) %>%  
  mutate(prob_cookery = cookery_count/feature_count) %>%  
  mutate(prob_travel = travel_count/feature_count)-> books_dowker_nest

#  add score column 
books_dowker_nest %>%  
  mutate(score = weight*feature_count) -> books_dowker_nest

# art words

find_target_words <- function(data, target_col, threshold) {
  words <- data %>%
    filter({{ target_col }} > threshold) %>%
    select(observations) %>%
    unnest(observations) %>%
    left_join(books_nonzero %>% as_tibble(rownames = "Word")) %>%
    select("Word") %>%
    unique()
  return(words)
}

# usage example: find art words with probability greater than 0.9
art_words <- find_target_words(books_dowker_nest, prob_art, 0.9)

# usage example: find biology words with probability greater than 0.9
biology_words <- find_target_words(books_dowker_nest, prob_biology, 0.9)

# usage example: find politics words with probability greater than 0.9
politics_words <- find_target_words(books_dowker_nest, prob_politics, 0.9)

# usage example: find cookery words with probability greater than 0.9
cooker_words <- find_target_words(books_dowker_nest, prob_cookery, 0.9)

# usage example: find travel words with probability greater than 0.9
travel_words <- find_target_words(books_dowker_nest, prob_travel, 0.9)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 12-  Finalized Data                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

books_dowker_nest %>%  
  select(c("observations", "weight", "difference", "score", "feature_count", "feature_pattern", "prob_art", "prob_politics", "prob_biology", "prob_cookery", "prob_travel")) %>%  
  unnest(feature_pattern)  %>%
  left_join(id_topics %>% mutate(Docs = row_number()),
            by = c(Docs = "Docs")
  )-> use_this_data

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 13-  Plots                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define function for creating boxplots
create_boxplot <- function(data, x_var) {
  ggplot(data, aes(x = {{x_var}}, y = as.factor(Topics))) + 
    geom_boxplot() +
    coord_flip() +
    theme_bw() + 
    ylab("Topics")
}

create_boxplot(use_this_data, prob_art) -> prob_art_box_plot
create_boxplot(use_this_data, prob_biology) -> prob_biology_box_plot
create_boxplot(use_this_data, prob_politics) -> prob_politics_box_plot
create_boxplot(use_this_data, prob_cookery) -> prob_cookery_box_plot
create_boxplot(use_this_data, prob_travel) -> prob_travel_box_plot

grid.arrange(prob_art_box_plot, prob_biology_box_plot, 
             prob_politics_box_plot, prob_cookery_box_plot, prob_travel_box_plot, 
             nrow = 2)


# Interpretation: 
# Each topic category is shifted up if y-axis is the probability of that topic. 
# Based on these boxplots we can observe that there is a separation between art, politics, biology books. 
# As measured by the probability of each topic, we can assume that Dowker Classifier is really separating this books.

# test
str(use_this_data)

aov_test_art <- aov(prob_art ~ as.factor(topics), use_this_data)
summary(aov_test_art)

aov_test_politics <- aov(prob_politics ~ as.factor(topics), use_this_data)
summary(aov_test_politics)

aov_test_biology <- aov(prob_biology ~ as.factor(topics), use_this_data)
summary(aov_test_biology)

aov_test_cookery <- aov(prob_cookery ~ as.factor(topics), use_this_data)
summary(aov_test_cookery)

aov_test_travel <- aov(prob_travel ~ as.factor(topics), use_this_data)
summary(aov_test_travel)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 14-  Dowker prob function by Dr. Robinson                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Compute Dowker posterior class probabilities from the output of dowker_nest
# 
# Input: df = output from dowker_nest
#        class_var = one of the column names in obs_var (used previously in 
#                    the call to dowker_nest) to identify classes of 
#                    observations
# Output: a new data frame with columns
#        feature_pattern = same as the input df
#        class_prob = nested column of tibbles containing two columns:
#              class_var = as before
#              prob      = posterior probability of class_var given 
#                          the feature_pattern

dowker_prob <- function(df,class_var){
  df %>% 
    unnest(observations) %>% 
    group_by(feature_pattern) %>% 
    count({{class_var}}) %>% 
    transmute(feature_pattern,{{class_var}},prob=n/sum(n)) %>% 
    nest(class_prob=c({{class_var}},prob)) %>% 
    ungroup()
}

dowker_prob(books_dowker_nest, Topics) -> dowker_prob_result

