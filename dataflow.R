source("data_retrieval.R")
source("preprocessing.R")
source("feature_construction.R")
source("training.R")

tweets = retrieve.tweets(n=1000)
tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
vc_tweets = Corpus(VectorSource(as.character(tweets$text)))
vc_tweets = process.tweets(vc_tweets)
tweets.tdm <- create.tdm(vc_tweets, training = T)
tweets.tdm.t <- as.data.frame(t(tweets.tdm), stringsAsFactors = F)
tweets.tdm.t["Class"] <- "love"
tweets.tdm.t[tweets.tdm.t$hate > 0, "Class"] <- "hate"
tweets.tdm.t <- tweets.tdm.t[setdiff(colnames(tweets.tdm.t), c("love", "hate"))]
tweets.trained <- trainModel(tweets.tdm.t, reset = T)

#tweets.trained$model
predictions <- predict(tweets.trained, tweets.tdm.t[,-ncol(tweets.tdm.t)])
summary(predictions)
table(predictions,tweets.tdm.t[,ncol(tweets.tdm.t)])

#
# Testing
#
testTweets <- retrieve.tweets(n=100)
testTweets$text <- sapply(testTweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
vc.testTweets = Corpus(VectorSource(as.character(testTweets$text)))
vc.testTweets = process.tweets(vc.testTweets)
testTweets.tdm <- create.tdm(vc.testTweets)
#testTweets.tdm <- testTweets.tdm[1:ncol(tweets.tdm)]
testTweets.tdm.t <- as.data.frame(t(testTweets.tdm), stringsAsFactors = F)
testTweets.tdm.t["Class"] <- "love"
testTweets.tdm.t[testTweets.tdm.t$hate > 0, "Class"] <- "hate"
testTweets.tdm.t <- testTweets.tdm.t[setdiff(colnames(testTweets.tdm.t), c("love", "hate"))]
testPrediction <- predict(tweets.trained, testTweets.tdm.t[,-ncol(testTweets.tdm.t)])
table(testPrediction,testTweets.tdm.t[,ncol(testTweets.tdm.t)])

testTweets.trained <- trainModel(testTweets.tdm.t)