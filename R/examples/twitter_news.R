# Script to analyze holiday destinations via twitter feeds
# See also https://rtweet.info/ for inspiration
library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(tm)

## Analyzing Twitter for Crete

rt <- read_rds("data/twitter/crete.rds")

## plot time series of tweets
rt %>%
  ts_plot("1 day") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #crete Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Who posted the most?
rt %>% 
  group_by(screen_name) %>%
  summarise(count = n()) %>%
  arrange(-count)

## Most important words
tweet_words <- rt %>% select(screen_name, created_at, text) %>% 
  unnest_tokens(word, text) %>%
  count(word, sort=T)
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words, by = "word")

top_25 <- tweet_words_interesting %>% 
  group_by(word) %>% 
  tally(sort=TRUE) %>% 
  slice(1:25)

top_25 %>% mutate(word = reorder(word, n, function(n) -n)) %>%
  ggplot() + geom_bar(aes(word, n), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("")

## Calc sentiment
bing_lex <- get_sentiments("bing")

sentiment_words <- rt %>% select(status_id, screen_name, created_at, text) %>% 
  unnest_tokens(word, text) %>% 
  inner_join(bing_lex, by = "word") 

sentiment_words %>% 
  mutate(created_at_day = lubridate::as_date(lubridate::round_date(created_at, "day")),
         sentiment_num = ifelse(sentiment == "positive", 1, -1), 
         count = n()) %>%
  ggplot() + 
  geom_bar(aes(created_at_day, fill = sentiment), stat = "count") + 
  facet_wrap(~sentiment, ncol = 1)


## Create word cloud
wordcloud2(top_25, color="random-light", size = .6, shuffle=T, rotateRatio = sample(c(1:100) / 100))


## TODO: Analyzing NYTimes (as with Twitter data)
rt_nytimes <- read_rds("data/twitter/crete_nytimes.rds")
View(rt_nytimes$data)
