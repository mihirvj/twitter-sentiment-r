library(RMOA)
library(kernlab)

trainModel <- function(tdm, reset=F) {
  hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
  tdm.fact <- factorise(tdm)
  tdm.stream <- datastream_dataframe(tdm.fact)
  model <- trainMOA(model = hdt, formula = Class ~ ., data = tdm.stream, reset = reset)
  return (model)
  #modelSVM <- ksvm(x=as.matrix(tdm[,-ncol(tdm)]), y=tdm[,ncol(tdm)], class.weights=c('love'=1,'hate'=5))
  #return (modelSVM)
}