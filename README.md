# Sentiment-Analysis
Sentiment Analysis in R on Jane Austen's book Pride and Prejudice for all words used more than 125 times in the book.

Output
![alt text](https://i.imgur.com/YzSoVUh.png "Logo Title Text 1")

```css
library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
tidy_data %>%
  filter(book == "prideprejudice") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

library(tidyverse)
bing <- get_sentiments("bing")
prideprejudice_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "prideprejudice" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)
counting_words %>%
  filter(n > 125) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")


```



