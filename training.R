library(RMOA)
#library(kernlab)
gl.fmla <- NULL
trainModel <- function(tweets.tdm.t, training=F, trainedModel = NULL) {
  tdm.fact <- factorise(tweets.tdm.t)
  tdm.stream <- datastream_dataframe(data=tdm.fact)
  if(training) {
    print("Creting new model")
    classVars <- setdiff(colnames(tweets.tdm.t), c('Class'))
    gl.fmla <<- as.formula(paste("Class ~ ", paste(classVars, collapse= "+")))
    hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", splitConfidence = "1")
    model <- trainMOA(model = hdt, formula = Class ~ ., data = tdm.stream, reset = T)
  }
  else {
    model <- trainMOA(model = trainedModel$model, formula = gl.fmla, data = tdm.stream, reset = F, chunksize = 1)
  }
  return (model)
  #modelSVM <- ksvm(x=as.matrix(tdm[,-ncol(tdm)]), y=tdm[,ncol(tdm)], class.weights=c('love'=1,'hate'=5))
  #return (modelSVM)
}
