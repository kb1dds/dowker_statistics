# Load libraries
library(gutenbergr) 
library(tidyverse)
library(tm)
library(tidytext)
library(ggthemes)

# Use Gutenberg book data
gutenberg_metadata <- gutenberg_metadata

# Download 100 data
gutenberg_15books <- gutenberg_download(c(5669, 147, 17306, 4776, 3207, 20125, 35894, 9596, 1341, 151, # Politics 10 
                                          7370, 612, 18, 2130, 6762, 1232, 20433, 39622, 34111, 15509, # Politics 20
                                          34645, 2176, 5000, 11242, 17408, 38532, 45504, 29904, 2398, 17373, # Art 10
                                          167, 20195, 3226, 5321, 3751, 5712, 13119, 16180, 17244, 22564, # Art 20
                                          1909, 2089, 2927, 20818, 18911, 6329, 14473, 27778, 2009, 4962, # Biology 10
                                          17966, 6052, 13249, 23755, 18350, 22764, 2938, 16410, 18884, 24456, # Biology 20 
                                          10011, 24384, 13265, 25631, 15019, 26032, 12815, 24205, 33974, 29730, # Cookery 10
                                          24542, 25007, 31534, 27245, 6978, 6429, 26558, 29329, 6385, 34822, # Cookery 20
                                          944, 1082, 3177, 3704, 6317, 39496, 12422, 15777, 22117, 28323, # Travel 10
                                          39806, 39685, 39615, 39474, 39308, 39082, 39026, 38203, 27250, 22409))

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
  bind_tf_idf(word, gutenberg_id, n) %>%  
  filter(tf_idf < 0.001) %>%  
  select(word) %>%  
  unique() -> stop_words_tf_idf

stop_words <- removeNumbers(stop_words_tf_idf$word) # remove numbers 

# gutenberg_id and text
gutenberg_15books %>% 
  group_by(gutenberg_id) %>% 
  slice_sample(n=500, replace = TRUE) %>% 
  unnest_tokens(word, text) %>%  
  summarize(text = reduce(word, paste)) -> books

# identify topics using logical values

# Vector of gutenberg_id values for each category
politics_ids <- c(5669, 147, 17306, 4776, 3207, 20125, 35894, 9596, 1341, 151, 7370, 612, 18, 2130, 6762, 1232, 20433, 39622, 34111, 15509)
art_ids <- c(34645, 2176, 5000, 11242, 17408, 38532, 45504, 29904, 2398, 17373, 167, 20195, 3226, 5321, 3751, 5712, 13119, 16180, 17244, 22564)
biology_ids <- c(1909, 2089, 2927, 20818, 18911, 6329, 14473, 27778, 2009, 4962, 17966, 6052, 13249, 23755, 18350, 22764, 2938, 16410, 18884, 24456)
cookery_ids <- c(10011, 24384, 13265, 25631, 15019, 26032, 12815, 24205, 33974, 29730, 24542, 25007, 31534, 27245, 6978, 6429, 26558, 29329, 6385, 34822)

# Mutate columns based on gutenberg_id values
books <- books %>%
  mutate(politics = gutenberg_id %in% politics_ids,
         art = gutenberg_id %in% art_ids,
         biology = gutenberg_id %in% biology_ids,
         cookery = gutenberg_id %in% cookery_ids)

# Mutate columns based on gutenberg_id values
logical_full_books <- books %>%
  mutate(category = case_when(
    gutenberg_id %in% politics_ids ~ "politics",
    gutenberg_id %in% art_ids ~ "art",
    gutenberg_id %in% biology_ids ~ "biology",
    gutenberg_id %in% cookery_ids ~ "cookery",
    TRUE ~ "travel"
  )) %>%
  select(-politics, -art, -biology, -cookery)

# Convert this text to a corpus: 
# Make a vector source from text (Convert vector to a Source object)
books_source <- VectorSource(logical_full_books$text)
books_corpus <- VCorpus(books_source)
books_corpus

clean_corpus <- function(corpus) {
  
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Break the list of stop words into chunks of manageable size (e.g., 100 words per chunk)
  stop_word_chunks <- split(stop_words, ceiling(seq_along(stop_words) / 100))
  
  # Apply cleaning steps for each chunk of stop words
  for (chunk in stop_word_chunks) {
    # Add more stopwords
    corpus <- tm_map(corpus, removeWords, words = c(chunk)) 
  }
  
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove numbers 
  corpus <- tm_map(corpus, removeNumbers)
  # Add stemming 
  corpus <- tm_map(corpus, stemDocument, language = "english")  
  
  return(corpus)
}

# Use clean_corpus function to the corpus: 


# Use clean_corpus function to the corpus: 
clean_books_corpus <- clean_corpus(books_corpus)

# Term Document Matrix:
# Apply TDM to "Books" corpus 
books_tdm <- TermDocumentMatrix(clean_books_corpus)
books_m <- as.matrix(books_tdm)

v3 <- sort(rowSums(books_m), decreasing=TRUE)
d3 <- data.frame(word = names(v3), freq=v3)

# Use inspect function to display detailed information on TDM
inspect(books_tdm)

# Identify non-zero values:
books_nonzero <- which(books_m != 0, arr.ind = TRUE)

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

# Apply Dowker Complex function to books_nonzero 
books_nonzero %>%  
  as_tibble() %>%  
  dowker_nest(Docs, Terms) -> books_dowker_nest

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
  mutate(difference = max - median)  %>%  # add difference column
  mutate(prob_art = art_count/feature_count) %>%  # probability of each topic by full count
  mutate(prob_biology = biology_count/feature_count)  %>%  
  mutate(prob_politics = politics_count/feature_count) %>%  
  mutate(prob_cookery = cookery_count/feature_count) %>%  
  mutate(prob_travel = travel_count/feature_count)  %>%  
  mutate(score = weight*feature_count) -> books_dowker_nest # add score column 

# Function to get words based on probability threshold
get_category_words <- function(data, prob_threshold, category_column, books_nonzero) {
  data %>%
    filter({{ category_column }} > prob_threshold) %>%
    select(observations) %>%
    unnest(observations) %>%
    left_join(books_nonzero %>% as_tibble(rownames = "Word")) %>%
    select("Word") %>%
    unique()
}

# Get words for each category
art_words <- get_category_words(books_dowker_nest, 0.9, prob_art, books_nonzero)
biology_words <- get_category_words(books_dowker_nest, 0.9, prob_biology, books_nonzero)
politics_words <- get_category_words(books_dowker_nest, 0.9, prob_politics, books_nonzero)
cooker_words <- get_category_words(books_dowker_nest, 0.9, prob_cookery, books_nonzero)
travel_words <- get_category_words(books_dowker_nest, 0.9, prob_travel, books_nonzero)

books_dowker_nest %>%  
  select(c("observations", "weight", "difference", "score", "feature_count", "feature_pattern", "prob_art", "prob_politics", "prob_biology", "prob_cookery", "prob_travel")) %>%  
  unnest(feature_pattern) %>%
  left_join(complete_data %>% mutate(Docs = row_number()),
            by = c(Docs = "Docs")
  ) -> use_this_data

# BoxPlot
use_this_data %>%
  pivot_longer(cols = starts_with("prob"), names_to = "Probability", values_to = "Value") %>%
  ggplot(aes(x = Value, y = as.factor(topics))) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  facet_wrap(~ Probability, scales = "free_x", ncol = 1)
