
#' Compute test error for given data using given model with given features.
#' Test data partition is randomply sampled.
#' @param data data that will be used for building model and predictin test error
#' @param build.model function that will build a model (should return dataframe with 'model, prediction, 'accuracy' columns)
#' @param features.for.model vector of string containing names of features to use for building model
#' @param predict.type[="response"] string representing predict type used in "predict" function for "type" parameter
#'    used when predicting target values using built model and test data
#' @return computed test error
compute.test.error <- function(data, build.model, features.for.model, predict.type = "response") {
  features.for.model <- unique(c(features.for.model, 'target'))
  # split data into training and test
  # set the seed to make your partition reproducible
  set.seed(123)
  
  percentage.data.for.training <- 0.75
  sample.size.training <- floor(percentage.data.for.training * nrow(data))
  training.indices <- sample(seq_len(nrow(data)), size = sample.size.training)

  # create training and test data
  training.data <- data[training.indices, features.for.model]
  test.data <- data[-training.indices, features.for.model]

  # build model
  build.model.result <- build.model(training.data, features.for.model)

  # get predictions from built model for test data
  test.predictions <- predict(build.model.result$model, type=predict.type, newdata=test.data)
  if (predict.type == "response") {
    test.predictions <- test.predictions$class
  }
  # compute test error
  test.error <- 1 - mean(test.predictions == test.data$target)

  return(test.error)
}

#' Compute test error for given data using given model with given features.
#' Test data partition uses all matches from last season.
#' @param data data that will be used for building model and predictin test error
#' @param build.model function that will build a model (should return dataframe with 'model, prediction, 'accuracy' columns)
#' @param features.for.model vector of string containing names of features to use for building model
#' @param predict.type[="response"] 
#'    string representing predict type used in "predict" function for "type" parameter
#'    used when predicting target values using built model and test data
#' @return computed test error
compute.test.error.using.last.season <- function(data, build.model, features.for.model, predict.type = "response") {
  features.for.model <- unique(c(features.for.model, 'target'))

  training.indices <- which(data$season != "2015/2016")

  # create training and test data
  training.data <- data[training.indices, features.for.model]
  test.data <- data[-training.indices, features.for.model]
  
  # build model
  build.model.result <- build.model(training.data, features.for.model)
  
  # get predictions from built model for test data
  test.predictions <- predict(build.model.result$model, type=predict.type, newdata=test.data)
  if (predict.type == "response") {
    test.predictions <- test.predictions$class
  }
  # compute test error
  test.error <- 1 - mean(test.predictions == test.data$target)
  
  return(test.error)
}