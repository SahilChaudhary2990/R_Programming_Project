# Sentiment Analysis on Tweets using R

# 1. Load Required Libraries
install.packages("rtweet")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tidyr")

library(rtweet)      # For accessing Twitter API
library(tidytext)    # For text mining and sentiment analysis
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(stringr)     # For string operations
library(tidyr)

# 2. Create Folders
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("plots")) dir.create("plots")

# 3. Search Parameters
keyword <- "India"
num_tweets <- 300

# 4. Simulated Tweets (Replace with search_tweets for live data)
tweets <- data.frame(
  status_id = 1:6,
  created_at = Sys.time(),
  screen_name = paste("user", 1:6, sep = ""),
  text = c(
    "Climate change is support. We need action now!",
    "Climate change is crisis. We need action now!",
    "Climate change is support. We need action now!",
    "Climate change is extinction. We need action now!",
    "Climate change is hopeful. We need action now!",
    "Climate change is extinction. We need action now!"
  ), stringsAsFactors = FALSE
)
write.csv(tweets, "data/tweets.csv", row.names = FALSE)

# 5. Preprocess Text
data("stop_words")
tweet_words <- tweets %>%
  select(status_id, text) %>%
  mutate(text = str_replace_all(text, "http\S+|@\S+|#[A-Za-z0-9_]+|[^[:alnum:] ]", "")) %>%
  unnest_tokens(word, text)

cleaned_tweets <- tweet_words %>%
  anti_join(stop_words, by = "word")

# 6. Perform Sentiment Analysis
bing_sentiments <- cleaned_tweets %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

# 7. Sentiment Count Plot
sentiment_count <- bing_sentiments %>%
  count(sentiment)

ggplot(sentiment_count, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("positive" = "#2ecc71", "negative" = "#e74c3c")) +
  theme_minimal() +
  labs(
    title = "Sentiment Distribution of Tweets",
    x = "Sentiment",
    y = "Word Count"
  )

# 8. Top Words Plot
top_words <- bing_sentiments %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  arrange(sentiment, desc(n))

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top Words by Sentiment",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()
