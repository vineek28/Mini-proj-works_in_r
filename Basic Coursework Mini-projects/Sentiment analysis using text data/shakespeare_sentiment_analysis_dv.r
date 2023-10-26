library(textdata)
library(readr)
library(dplyr)
library(e1071)
library(mlbench)

#Text mining packages
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")
#1 load the data 
View(shakespeare)
str(shakespeare)

#2 Pipe the shakespeare data frame to the next line
#use count to find out how may titles/types are there 
#20bds0387
shakespeare %>% 
  count(title, type)

#3 Load tidytext/tidyverse 
#20bds0387
library(tidytext)
library(dplyr)


#4. create and object tidy_shakespeare
# Group by the titles of the plays
# Define a new column linenumber
# Transform the non-tidy text data to tidy text data
#20bds0387
tidy_shakespeare <- shakespeare %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>% # Transform the non-tidy text data to tidy text data
  ungroup()
tidy_shakespeare

#5.Pipe the tidy Shakespeare data frame to the next line
# Use count to find out how many times each word is used
#20bds0387
tidy_shakespeare %>% 
  count(word, sort = TRUE)

#6.Sentiment analysis of tidy_shakespeare assin to object shakespeare_sentiment
# Implement sentiment analysis with the "bing" lexicon
#20bds0387
shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) 
shakespeare_sentiment

#7.shakespeare_sentiment
# Find how many positive/negative words each play has
#20bds0387
shakespeare_sentiment %>%
  count(title, sentiment)

#8. Tragedy or comedy from tidy_shakespeare  assign to sentiment_counts
# Implement sentiment analysis using the "bing" lexicon
# Count the number of words by title, type, and sentiment
#20bds0387
sentiment_counts <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>% 
   count(title,sentiment)
sentiment_counts

#9. from sentiment_counts
# Group by the titles of the plays
# Find the total number of words in each play
# Calculate the number of words divided by the total
# Filter the results for only negative sentiment then arrange percentage in asc order
#20bds0387

sentiment_counts %>%
  group_by(title) %>% 
  mutate(total = sum(n),
         percent = n / total) %>% 
  filter(sentiment == "negative") %>% 
  arrange(percent)

#10 Most common positive and negative words and assign to word_could
# Implement sentiment analysis using the "bing" lexicon
# Count by word and sentiment
#20bds0387
word_counts <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)
word_counts

#11. extract top 10 words from word_counts and assing to top_words
# Group by sentiment
# Take the top 10 for each sentiment and ungroup it
# Make word a factor in order of n
#20bds0387
top_words <- word_counts %>%
  group_by(sentiment) %>% # Group by sentiment
  top_n(10) %>%  # Take the top 10 for each sentiment
  ungroup() %>%  # Make word a factor in order of n
  mutate(word = reorder(word, n))
top_words

#12 Use aes() to put words on the x-axis and n on the y-axis
# Make a bar chart with geom_col()
# facet_wrap for sentiments and apply scales  as free
#Move x to y and y to x
# Use aes() to put words on the x-axis and n on the y-axis
#20bds0387
library(ggplot2)
ggplot(top_words, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()+
  ggtitle("plot by vineeth krishna")
  

#13. from tidy_shakespeare Calculating a contribution score
# Count by title and word
# Implement sentiment analysis using the "afinn" lexicon
# Group by title
# Calculate a contribution for each word in each title
#20bds0387

sentiment_contributions <- tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%  
  inner_join(get_sentiments("afinn")) %>% 
  group_by(title) %>%  
  mutate(contribution = (n * value) / sum(n)) %>%
  ungroup()
View(sentiment_contributions)
View(get_sentiments("afinn"))
  


tidyshakespeare_reduced <- head(tidy_shakespeare,  n = 5000)

#word cloud 
corpus = Corpus(VectorSource(tidyshakespeare_reduced$word))
corpus[[3]][1]

#Conversion to Lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#Removing Punctuation
corpus = tm_map(corpus, removePunctuation)

#Remove stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))

# Stemming
corpus = tm_map(corpus, stemDocument)

# Eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)
corpus[[2]][1] 


DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)
head(dat, 5)

set.seed(100)
wordcloud(words = dat$word, freq = dat$freq, min.freq = 3, max.words=250, random.order=FALSE, rot.per=0.30, colors=brewer.pal(8, "Dark2"))
