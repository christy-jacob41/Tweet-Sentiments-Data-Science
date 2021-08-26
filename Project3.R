install.packages(c("rtweet", "ggplot2", "dplyr","igraph", "ggraph", "rjson", "zoo"  ), dependencies = TRUE)

# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# plotting packages
library(igraph)
library(ggraph)
library(rjson)
library(jsonlite)
library(zoo)
library(dplyr)
library(sentimentr)
library(tidyverse)

getSources()

??VCorpus
gw_tweets <- search_tweets(q = "#globalwarming", n = 10000,
                               lang = "en",
                               include_rts = FALSE)

gw_tweets$stripped_text <- gsub("http.*","",  gw_tweets$text)
gw_tweets$stripped_text <- gsub("https.*","", gw_tweets$stripped_text)

sentences <- get_sentences(gw_tweets$text)

# viweing tweet 1 and getting sentiment of sentences in tweet 1
sentences[[1]]
sentiment(sentences[[1]])

# viewing the attributes of the sentiment in tweet 1
sentiment_attributes(sentences[[1]])

# getting the overall sentiment of all global warming tweets found
overall_sentiment <- sentiment_by(gw_tweets$text)
# summary statistics of the average sentiment of each tweet
summary(overall_sentiment$ave_sentiment)
# histogram of the average sentiment of each tweet
qplot(overall_sentiment$ave_sentiment,geom="histogram",binwidth=0.2,main="Overall Sentiment")

# plotted word count by average sentiment to see if there is a correlation
plot(overall_sentiment$word_count, overall_sentiment$ave_sentiment)
