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
access_token <- hidden'
access_secret <- 'hidden'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
2

# this API can not download more than 11222 tweets, twitter will "rate limit" the API when doing so
tiffany_tweets = searchTwitter("tiffany+co", n=11222, lang="en")
save(tiffany_tweets, file = "tweets.Rdata")

object.size(tiffany_tweets)
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
tiffany_txt = gsub("tiffany", "", tiffany_txt)
tiffany_txt = gsub("amp", "", tiffany_txt)
tiffany_txt <- tm_map(tiffany_txt, removeWords("tiffany", "amp", "co"))
object.size(tiffany_txt)
wordcloud(tiffany_txt)
getwd()
png("tiffany_wordcloud.png", width=6, height=4, units="in", res=600)
wordcloud(tiffany_txt, random.order = F, max.words = 300, scale = c(3, 0.5), colors = rainbow(20) )
dev.off()

##tiffany_emo = classify_emotion(tiffany_txt, algorithm="bayes", prior=1.0)
tiffany_emo <- sentiment(tiffany_txt)
tiffany_emo_df <- data.frame(tiffany_txt, polarity = tiffany_emo$polarity)
tiffany_emo_df[1,]

png("tiffany_emotion.png", width=8, height=6, units="in", res=600)
ggplot(tiffany_emo_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) +scale_fill_brewer(palette="RdGy") +labs(x="polarity categories", y="number of tweets") +labs(title = "Sentiment Analysis of Tweets about Tiffany & co\n(classification by polarity)",plot.title = element_text(size=12))
dev.off()

barplot(tiffany_emo_df)


tiffany_corpus <- Corpus(VectorSource((tiffany_txt)))
tiffany_corpus

tiffany_clean <- tm_map(tiffany_corpus, removeWords() )
wordcloud(tiffany_txt)









