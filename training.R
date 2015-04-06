library(RMOA)
gl.fmla <- NULL

#
# trains and updates the model in training and testing phases
# input:
#       tweets.tdm.t - a data frame with terms as columns and tweet documents as rows
#       training - a logical indicating whether to train model discarding previous changes
#                  or updating the model. TRUE indicates that the model must be trained 
#                  discarding old data. Defaults to FALSE.
#       trainedModel - In case of updating the model, this parameter acts as the model that
#                      needs to be updated. Pass the model received from the same function call
#                      in training phase as this parameter. Defaults to NULL
# returns:
#       A trained or updated model as per the parameters passed
#
trainModel <- function(tweets.tdm.t, training=F, trainedModel = NULL) {
  # factorise the data set
  tdm.fact <- factorise(tweets.tdm.t)
  # create a stream of the data set
  tdm.stream <- datastream_dataframe(data=tdm.fact)
  # in case of training phase, previous data is discarded
  if(training) {
    # create formula based on the feature set i.e. the columns of the data set
    classVars <- setdiff(colnames(tweets.tdm.t), c('Class'))
    gl.fmla <<- as.formula(paste("Class ~ ", paste(classVars, collapse= "+")))
    # create a HoeffdingTree model
    hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", splitConfidence = "1")
    # train the model using training data
    model <- trainMOA(model = hdt, formula = Class ~ ., data = tdm.stream, reset = T)
  }
  # in case of testing, previous model is remembered
  else {
    # train model using previous model
    model <- trainMOA(model = trainedModel$model, formula = gl.fmla, data = tdm.stream, reset = F, chunksize = 1)
  }
  return (model)
}
