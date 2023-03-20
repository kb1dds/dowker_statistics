
## List Dependencies
## Make sure to download ....

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0-  Loading Libraries                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pacman)

## Loading required packages and install them if not yet installed 
p_load(gutenbergr, tidyverse, tm, tidytext, ggthemes)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.1 -  Loading data                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Use Gutenberg book data
gutenberg_metadata <- gutenberg_metadata

# save as csv file 
# "gutenberg_download" function is to download works using a project Gutenberg ID.
# Use gutenberg text data topics in Politics, Art, Biology, Cookery, Travel and combine them together:
gutenberg_15books <- gutenberg_download(c(5669, 147, 17306, 4776, 3207, 20125, 35894, 9596, 1341, 151, # Politics 10 
                                          7370, 612, 18, 2130, 6762, 1232, 20433, 39622, 34111, 15509, # Politics 20
                                          34645, 2176, 5000, 11242, 17408, 38532, 45504, 29904, 2398, 17373, # Art 10
                                          167, 20195, 3226, 5321, 3751, 5712, 13119, 16180, 17244, 22564, # Art 20
                                          1909, 2089, 2927, 20818, 18911, 6329, 14473, 27778, 2009, 4962, # Biology 10
                                          17966, 6052, 13249, 23755, 18350, 22764, 2938, 16410, 18884, 24456, # Biology 20 
                                          10011, 24384, 13265, 25631, 15019, 26032, 12815, 24205, 33974, 29730, # Cookery 10
                                          24542, 25007, 31534, 27245, 6978, 6429, 26558, 29329, 6385, 34822, # Cookery 20
                                          944, 1082, 3177, 3704, 6317, 39496, 12422, 15777, 22117, 28323, # Travel 10
                                          39806, 39685, 39615, 39474, 39308, 39082, 39026, 38203, 27250, 22409), # Travel 20 
                                        mirror = "http://mirrors.xmission.com/gutenberg/")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.2 - Documents, Topics (2 columns)                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data <- gutenberg_15books %>% 
  mutate(Topics = case_when(
  gutenberg_id %in% c(5669, 147, 17306, 4776, 3207, 20125, 35894, 9596, 1341, 151, 7370, 612, 18, 2130, 6762, 1232, 20433, 39622, 34111, 15509) ~ "Politics",
  gutenberg_id %in% c(34645, 2176, 5000, 11242, 17408, 38532, 45504, 29904, 2398, 17373, 167, 20195, 3226, 5321, 3751, 5712, 13119, 16180, 17244, 22564) ~ "Art",
  gutenberg_id %in% c(1909, 2089, 2927, 20818, 18911, 6329, 14473, 27778, 2009, 4962, 17966, 6052, 13249, 23755, 18350, 22764, 2938, 16410, 18884, 24456) ~ "Biology",
  gutenberg_id %in% c(10011, 24384, 13265, 25631, 15019, 26032, 12815, 24205, 33974, 29730, 24542, 25007, 31534, 27245, 6978, 6429, 26558, 29329, 6385, 34822) ~ "Cookery", 
  gutenberg_id %in% c(944, 1082, 3177, 3704, 6317, 39496, 12422, 15777, 22117, 28323,39806, 39685, 39615, 39474, 39308, 39082, 39026, 38203, 27250, 22409) ~ "Travel", 
  TRUE ~ NA_character_ # for cases not covered by any of the above conditions
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2-  tf-idf analysis                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

book_words <- data %>%  
  unnest_tokens(word, text) %>%  
  count(Topics, gutenberg_id, word, sort = TRUE) %>%  
  ungroup()

total_words <- book_words %>%  
  group_by(Topics, gutenberg_id) %>%  
  summarize(total = sum(n))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3-  Create New Stop Word List using tf-idf                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

left_join(book_words, total_words) %>%  
  bind_tf_idf(word, gutenberg_id, n) %>%  
  filter(tf_idf < 0.001) %>%  
  select(word) %>%  
  unique() -> stop_words_tf_idf

stop_words <- removeNumbers(stop_words_tf_idf$word) # remove numbers 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4-  gutenberg_id and text                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data %>% 
  group_by(gutenberg_id, Topics) %>% 
  slice_sample(n=500, replace = TRUE) %>% 
  unnest_tokens(word, text) %>%  
  summarize(text = reduce(word, paste)) -> books

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5-  Convert this text to a corpus   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Use different way 

# Make a vector source from text (Convert vector to a Source object)
books_source <- VectorSource(books$text)
books_corpus <- VCorpus(books_source)

# Function that cleans corpus: 
clean_corpus <- function(corpus) {
  
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stop_words)) 
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove numbers 
  corpus <- tm_map(corpus, removeNumbers)
  # Add stemming 
  corpus <- tm_map(corpus, stemDocument, language = "english")  
  
  return(corpus)
}

# Use clean_corpus function to the corpus: 
clean_books_corpus <- clean_corpus(books_corpus)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7-  Apply Term Document Matrix to Books corpus                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

books_tdm <- TermDocumentMatrix(books_corpus)
books_m <- as.matrix(books_tdm)

v3 <- sort(rowSums(books_m), decreasing=TRUE)
d3 <- data.frame(word = names(v3), freq=v3)

# Use inspect function to display detailed information on TDM
inspect(books_tdm)

# Identify non-zero values:
books_nonzero <- which(books_m != 0, arr.ind = TRUE)

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

### Change later 
new_df

# Apply Dowker Complex function to books_nonzero 
# books_nonzero %>%  
#   as_tibble() %>%  
#   dowker_nest(Docs, Terms) -> books_dowker_nest

### change later
dowker_nest(new_df,c(Docs, Topics), Terms) -> books_dowker_nest

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
books_dowker_nest %>%  
  filter(prob_art > 0.9) %>%  
  select(observations) %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Word")) %>%  
  select("Word") %>%  
  unique() -> art_words

# biology words
books_dowker_nest %>%  
  filter(prob_biology > 0.9) %>%  
  select(observations) %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Word")) %>%  
  select("Word") %>%  
  unique() -> biology_words

# politics words
books_dowker_nest %>%  
  filter(prob_politics > 0.9) %>%  
  select(observations) %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Word")) %>%  
  select("Word") %>%  
  unique() -> politics_words

# cookery words
books_dowker_nest %>%  
  filter(prob_cookery > 0.9) %>%  
  select(observations) %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Word")) %>%  
  select("Word") %>%  
  unique() -> cooker_words

# politics words
books_dowker_nest %>%  
  filter(prob_travel > 0.9) %>%  
  select(observations) %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Word")) %>%  
  select("Word") %>%  
  unique() -> travel_words

# make new column called topics in order to use it later
logical_full_books%>%  
  filter(politics %in% TRUE) %>%  
  mutate(topics = "politics") -> politics_data
logical_full_books %>%  
  filter(art %in% TRUE) %>%  
  mutate(topics = "art") -> art_data
logical_full_books %>%  
  filter(biology %in% TRUE) %>%  
  mutate(topics = "biology") -> biology_data
logical_full_books %>%  
  filter(cookery %in% TRUE) %>%  
  mutate(topics = "cookery") -> cookery_data
logical_full_books %>%  
  filter(politics %in% FALSE & art %in% FALSE & biology %in% FALSE & cookery %in% FALSE) %>%  
  mutate(topics = "travel") -> travel_data

# combine three data into one 
politics_data %>%  
  rbind(art_data) %>%  
  rbind(biology_data) %>%  
  rbind(cookery_data) %>% 
  rbind(travel_data) -> complete_data

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 12-  Finalized Data                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

books_dowker_nest %>%  
  select(c("observations", "weight", "difference", "score", "feature_count", "feature_pattern", "prob_art", "prob_politics", "prob_biology", "prob_cookery", "prob_travel")) %>%  
  unnest(feature_pattern) %>%
  left_join(complete_data %>% mutate(Docs = row_number()),
            by = c(Docs = "Docs")
  ) -> use_this_data

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 13-  Plots                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ggplot(use_this_data, aes(x = prob_art)) +
  geom_histogram()

ggplot(use_this_data, aes(x = prob_biology)) +
  geom_histogram()

ggplot(use_this_data, aes(x = prob_politics)) +
  geom_histogram()

ggplot(use_this_data, aes(x = prob_cookery)) +
  geom_histogram()

ggplot(use_this_data, aes(x = prob_travel)) +
  geom_histogram()

# weight vs feature count 
ggplot(use_this_data, aes(x = weight, y = feature_count)) + 
  geom_point() + 
  theme_bw()

# Scatterplot (score vs difference) -> see the trend, explore, outliers, what they mean??? 
ggplot(use_this_data, aes(x = difference, y = score, color = weight, size = feature_count)) + 
  geom_point() + 
  theme_bw()

# Scatterplot (score vs difference) 
ggplot(use_this_data, aes(x = difference, y = score)) + 
  geom_point() + 
  theme_bw()

# "Weight" vs "Each topic probability"
ggplot(use_this_data, aes(y=weight, x=prob_art))+
  geom_point()+ 
  theme_bw()

ggplot(use_this_data, aes(y=weight, x=prob_biology))+
  geom_point()+
  theme_bw()

ggplot(use_this_data, aes(y=weight, x=prob_politics))+
  geom_point()+
  theme_bw()

ggplot(use_this_data, aes(y=weight, x=prob_cookery))+
  geom_point()+
  theme_bw()

ggplot(use_this_data, aes(y=weight, x=prob_travel))+
  geom_point()+
  theme_bw()

# Boxplot (prob art VS topics)
use_this_data %>%  
  ggplot(aes(x = prob_art, y = as.factor(topics))) + 
  geom_boxplot() + 
  coord_flip()+ 
  theme_bw() 

# Boxplot (prob biology VS topics)
use_this_data %>% 
  ggplot(aes(x = prob_biology, y = as.factor(topics))) + 
  geom_boxplot() + 
  coord_flip()+
  theme_bw()

# Boxplot (prob politics VS topics)
use_this_data %>% 
  ggplot(aes(x = prob_politics, y = as.factor(topics))) + 
  geom_boxplot() + 
  coord_flip() +
  theme_bw()

# Boxplot (prob cookery VS topics)
use_this_data %>% 
  ggplot(aes(x = prob_cookery, y = as.factor(topics))) + 
  geom_boxplot() + 
  coord_flip() +
  theme_bw()

# Boxplot (prob travel VS topics)
use_this_data %>% 
  ggplot(aes(x = prob_travel, y = as.factor(topics))) + 
  geom_boxplot() + 
  coord_flip() +
  theme_bw()


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


###### tf-idf VS Dowker Probability ##### 
use_this_data %>%  
  unnest(observations) %>%  
  left_join(books_nonzero %>%  
              as_tibble(rownames = "Words")) %>%  
  left_join(book_words,by = c(Words = "word"))  %>%  
  ggplot(aes(x =tf_idf, y = prob_art)) + 
  geom_point()+
  theme_bw()

# histogram of prob of each one of these topics 
use_this_data %>%  
  filter(topics == "politics") %>% 
  ggplot(aes(x = prob_art)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "politics") %>% 
  ggplot(aes(x = prob_biology)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "politics") %>% 
  ggplot(aes(x = prob_cookery)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "politics") %>% 
  ggplot(aes(x = prob_travel)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "art") %>% 
  ggplot(aes(x = prob_politics)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "art") %>% 
  ggplot(aes(x = prob_biology)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "art") %>% 
  ggplot(aes(x = prob_cookery)) + 
  geom_histogram() + 
  theme_bw()

use_this_data %>%  
  filter(topics == "art") %>% 
  ggplot(aes(x = prob_travel)) + 
  geom_histogram() + 
  theme_bw()

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

dowker_prob(books_dowker_nest, topics) -> df_df

# dowker_graph in dowker split



