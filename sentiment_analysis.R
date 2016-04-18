require(RCurl)
require(tm)
require(wordcloud)
require(devtools)
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(sentiment)

consumer_key <- 'Gvb1JduVEjUyCxci2RhG32eAR'
consumer_secret <- 'V19jwij6feZMF87GLeMSgFvK2QH4JZbiVAwgIXsW0WCD5aljc2'
access_token <- '721483439673839616-6zCsCkUH98E5DFiK4OElSnPJdwpeuoP'
access_secret <- 'nTKdYVoAYb0ScJayJNPWVB7hzWmpffU6C0sjyz24RZG68'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
2

# harvest some tweets
tiffany_tweets = searchTwitter("tiffany+co", n=1500, lang="en")

# get the text
tiffany_txt = sapply(tiffany_tweets, function(x) x$getText())
tiffany_txt

# remove retweet entities
tiffany_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tiffany_txt)
# remove at people
tiffany_txt = gsub("@\\w+", "", tiffany_txt)
# remove punctuation
tiffany_txt = gsub("[[:punct:]]", "", tiffany_txt)
# remove numbers
tiffany_txt = gsub("[[:digit:]]", "", tiffany_txt)
# remove html links
tiffany_txt = gsub("http\\w+", "", tiffany_txt)
# remove unnecessary spaces
tiffany_txt = gsub("[ \t]{2,}", "", tiffany_txt)
tiffany_txt = gsub("^\\s+|\\s+$", "", tiffany_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
tiffany_txt = sapply(tiffany_txt, try.error)

# remove NAs in some_txt
tiffany_txt = tiffany_txt[!is.na(tiffany_txt)]
names(tiffany_txt) = NULL

class(tiffany_txt)
#remove duplicates of tweets
tiffany_txt <- unique(tiffany_txt)
tiffany_corpus <- Corpus(VectorSource((tiffany_txt)))
tiffany_corpus

tiffany_clean <- tm_map(tiffany_corpus, removeWords() )
wordcloud(tiffany_txt)









