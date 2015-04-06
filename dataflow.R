source("data_retrieval.R")
source("preprocessing.R")
source("feature_construction.R")
source("training.R")

# retrive tweets and preprocess
tweets = retrieve.tweets(n=1000)
tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
vc_tweets = Corpus(VectorSource(as.character(tweets$text)))
vc_tweets = process.tweets(vc_tweets)

# create Document-Term Matrix
tweets.tdm <- DocumentTermMatrix(vc_tweets, control=list(minWordLength=1))

# separate into training and test data 
index <- sample(1:dim(tweets.tdm)[1])
tweets.tdm.training <- tweets.tdm[index[1:floor(dim(tweets.tdm)[1]/2)],]
tweets.tdm.test <- tweets.tdm[index[((ceiling(dim(tweets.tdm)[1]/2)) + 1):dim(tweets.tdm)[1]],]

# extract features from training data
tweets.tdm.df <- extract.features(tweets.tdm.training)

# assign 'Class' to tweets
tweets.tdm.df["Class"] <- "love"
tweets.tdm.df[tweets.tdm.df$hate > 0, "Class"] <- "hate"

# removes 'love' and 'hate' from the feature set
tweets.tdm.df <- tweets.tdm.df[setdiff(colnames(tweets.tdm.df), c("love", "hate"))]

# train model using training data
tweetModel <- trainModel(tweets.tdm.df, training = T)

# test model on training data
predictions <- predict(tweetModel, newdata = tweets.tdm.df, type = "response")
summary(predictions)
table(predictions,tweets.tdm.df[,ncol(tweets.tdm.df)])

#
# Testing using test data that was set aside initially
# convert test data (Document-term matrix) into data frame
#
test.matrix <- as.matrix(tweets.tdm.test)
tweets.tdm.test <- as.data.frame(test.matrix)

# add 'Class' to test data
tweets.tdm.test["Class"] <- "love"
tweets.tdm.test[tweets.tdm.test$hate > 0, "Class"] <- "hate"

# remove 'love' and 'hate' from feature set
tweets.tdm.test <- tweets.tdm.test[setdiff(colnames(tweets.tdm.test), c("love", "hate"))]

# predict using our model for test data
chunk <- 50
for(i in seq(1, dim(tweets.tdm.test)[1], chunk)) {
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  dataset <- tweets.tdm.test[seq(i:i+chunk-1),]
  # predict test tweets using model
  prediction <- predict(tweetModel, newdata = dataset, type = "response")
  # update model using test tweets
  tweetModel <- trainModel(dataset, training = F, trainedModel = tweetModel)
  #
  # create confusion matrix and find accuracy
  #
  cm <- table(prediction,dataset[,ncol(dataset)])
 
  # if there is a single class in the prediction model (confusion matrix)
  if(nrow(cm) == 1) {
    if(!is.na(cm['love'])) {
      tp <- cm['love', 'love']
      fp <- cm['love', 'hate']
    } 
    else {
      tn <- cm['hate', 'hate']
      fn <- cm['hate', 'love']
    }
  }
  # for two classes in the matrix
  else {
    tp <- cm['love', 'love']
    fp <- cm['love', 'hate']
    tn <- cm['hate', 'hate']
    fn <- cm['hate', 'love']
  }
  # calculate accuracy of current prediction
  accuracy = (tp + tn) / (tp + tn + fp + fn)
  print(paste('running iteration ', i, ' accuracy: ', accuracy))
}