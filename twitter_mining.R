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

consumer_key <- 'hidden'
consumer_secret <- 'hidden'
access_token <- 'hidden'
access_secret <- 'hidden'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
2

####################################################################################
# function declaration
#####################################################################################
getTweets <- function(keyword, number){
  tweets <- searchTwitter(keyword, n=number, lang="en")
  return(tweets)
}



tweet_emo_df <- function(keyword, tweet_txt){
  emo <- sentiment(tweet_txt)
  emo_df <- data.frame(keyword, tweet_txt, polarity = emo$polarity)
  return(emo_df)
}

#get tweets for a list of keywords (in this case, a list of fashion copanies)
getTweets_for_list <- function(namelist, number){
  tweet_list <- list()
  for(name in namelist){
    tweet_list[[name]] <- getTweets(name, number)
  }
  return(tweet_list)
}
##clean up a list of tweets
cleanupTweets <- function(raw_tweets, keyword){
  # get the text
  tweet_txt = sapply(raw_tweets, function(x) x$getText())
  
  # remove retweet entities
  tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_txt)
  # remove at people
  tweet_txt = gsub("@\\w+", "", tweet_txt)
  # remove punctuation
  tweet_txt = gsub("[[:punct:]]", "", tweet_txt)
  # remove numbers
  tweet_txt = gsub("[[:digit:]]", "", tweet_txt)
  # remove html links
  tweet_txt = gsub("http\\w+", "", tweet_txt)
  # remove unnecessary spaces
  tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
  tweet_txt = gsub("^\\s+|\\s+$", "", tweet_txt)
  
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
  tweet_txt = sapply(tweet_txt, try.error)
  
  # remove NAs in some_txt
  tweet_txt = tweet_txt[!is.na(tweet_txt)]
  names(tweet_txt) = NULL
  
  #remove duplicates of tweets
  tweet_txt <- unique(tweet_txt)
  for(word in keyword){
    tweet_txt = gsub(word, "", tweet_txt)
  }

  return(tweet_txt) 
}
## clean up a list of list of tweets
cleanupTweets_list <- function(tweet_list){
  tweet_list_clean <- list()
  for(tweet in names(tweet_list)){
    tweet_list_clean[[ tweet ]] <- cleanupTweets(tweet_list[[tweet]], tweet)
  }
  return(tweet_list_clean)
}
#classify tweet emotions
sentiment_emo_list <- function(tweet_list_clean){
  tweet_list_emo <- list()
  for(tweet in names(tweet_list_clean)){
    emo <- sentiment::classify_emotion(tweet_list_clean[[tweet]])
    tweet_list_emo[[ tweet ]] <- emo[,7]
  }
  return(tweet_list_emo)
}
#clasify polarity for tweets
sentiment_polarity_list <- function(tweet_list_clean){
  tweet_list_polarity <- list()
  for(tweet in names(tweet_list_clean)){
    emo <- sentiment::classify_polarity(tweet_list_clean[[tweet]])
    tweet_list_polarity[[ tweet ]] <- emo[,4]
  }
  return(tweet_list_polarity)
}

emo_classificaition <- list("anger", "disgust", "joy", "fear", "sadness", "surprise")

emo_ratio <- function(tweet_list){
  emo_matrix <- array(0, c(6, length(names(tweet_list))))
  rownames(emo_matrix) <- c("anger", "disgust", "joy", "fear", "sadness", "surprise")
  colnames(emo_matrix) <- names(tweet_list)
  
  for(tweet in names(tweet_list)){
    if(sum(is.na(tweet_list[[tweet]])) != length(tweet_list[[tweet]])){
      not_na <- tweet_list[[tweet]] [!is.na(tweet_list[[tweet]]) ]
      not_na_table <- table(not_na)
      emo_matrix[names(not_na_table), tweet] <- not_na_table/sum(not_na_table)
      
    }
  }
  return(emo_matrix)
}

polarity_ratio <- function(tweet_list){
  polar_matrix <- array(0, c(3, length(names(tweet_list))))
  rownames(polar_matrix) <- c("positive", "neutral", "negative")
  colnames(polar_matrix) <- names(tweet_list)
  
  for(tweet in names(tweet_list)){
    not_na <- tweet_list[[tweet]]
    polar_table <- table(not_na)
    polar_matrix[names(polar_table), tweet] <- polar_table/sum(polar_table)
  }
  return(polar_matrix)  
}

################################################################################
#      sentimental analysis of a list of namebrands
##################################################################################
fashion_brand <- c("Tiffany & Co", "Prada","Dolce & Gabbana", "Gucci", "Armani","Chanel", "Louis Vuitton", "Hermes")

#get 1000 tweets for each of the 8 fashion companies
#streamming more than 10K tweets would usually get blocked by twitter
fashion_tweet <- getTweets_for_list(fashion_brand, 10000)
#cleaning up the tweets(numbers, retweet, hashtag, username, etc)
fashion_tweet_clean <- cleanupTweets_list(fashion_tweet)

#classify the emotions of the tweets
fashion_tweet_emo <- sentiment_emo_list(fashion_tweet_clean)
#calculate the percentage of different emotions
fashion_tweet_emo_ratio <- emo_ratio(fashion_tweet_emo)

#classify the polarity of the tweets, e.g. positive or negative
fashion_tweet_polarity <- sentiment_polarity_list(fashion_tweet_clean)
#calculate the percentage of different polarity
fashion_tweet_polarity_ratio <- polarity_ratio(fashion_tweet_polarity)

barplot(fashion_tweet_emo_ratio,xlim = c(0,12),ylim = c(0,1.2),legend = row.names(fashion_tweet_emo_ratio), col = rainbow(6),args.legend = list(x = "topright", bty = "n", inset=c(-0.1, 0))  )
title("emo-classification of consumer tweets on Fashion companies (80K tweets)")

barplot(fashion_tweet_polarity_ratio,xlim = c(0,12),ylim = c(0,1.2),legend = row.names(fashion_tweet_polarity_ratio), col = rainbow(3), args.legend = list(x = "topright", bty = "n", inset=c(-0.1, 0)) )
title("positive/negative refection of consumer tweets on Fashion companies (80K tweets)")

###########################################################################################
###              wordcloud generation example
###########################################################################################
tiffany_tweet <- searchTwitter("tiffany+co", 5000)
tiffany_clean <- cleanupTweets(tiffany_tweet, c("tiffany", "amp", "co"))

png("tiffany_wordcloud.png", width=6, height=4, units="in", res=600)
wordcloud(tiffany_clean, random.order = F, max.words = 300, scale = c(3, 0.5), colors = rainbow(20) )
dev.off()




