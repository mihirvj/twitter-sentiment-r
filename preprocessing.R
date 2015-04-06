library(tm)
# requires SnowballC
#
# pre-process tweets recieved from twitter. pre-processing tasks include
# conbersion to lowercase, removal of punctuation, numbers and stop-words
# and stemming the document
# input: 
#       vc_tweets - a Corpus object containing tweets as documents
# returns: 
#       a Corpus object after performing above mentioned pre-processing tasks
#
process.tweets = function(vc_tweets) {
  # convert to lower case
  vc_tweets = tm_map(vc_tweets, content_transformer(tolower), lazy = TRUE)
  # remove punctuation
  vc_tweets = tm_map(vc_tweets, removePunctuation)
  # remove numbers
  vc_tweets = tm_map(vc_tweets, removeNumbers)
  # remove stop words
  vc_tweets <- tm_map(vc_tweets, removeWords, c(stopwords("english"),"break", "next", "new"))
  # stem document
  vc_tweets <- tm_map(vc_tweets, stemDocument)
  
  return (vc_tweets)
}