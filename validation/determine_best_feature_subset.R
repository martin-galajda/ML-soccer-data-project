library(hash)
## hash-2.2.6 provided by Decision Patterns
source('./validation/cross_validate.R')

#' Determine best subset of errors by tring computing cross-validation error on every possible combination of the features.
#' @param data.for.training data that will be used for building model and predictin test error
#' @param features.for.predicting vector of strings containing names of features that can be used for building model
#' @param build.model function which builds model
#'     function Args:
#'         - data.for.training -- dataframe containing data for training
#'         - features.for.predicting - vector of strings containing names of features that can be used for building model
#'     Returns:
#          - list which contains built model under key "model" (e.g. result[['model]])
#' @return information about best subset of features in form of hash dictionary
determine.best.subset.of.features <- function(data.for.training, features.for.predicting, build.model, predict.type = "response", repeat.k.fold) {
  result <- hash()
  lowest.CV.error.seen <- 1.0
  
  combinations.length <- length(features.for.predicting)
  
  best.subset.features <- NULL
  
  for (number.of.features.to.select in 1:combinations.length) {
    all.possible.combinations <- combn(features.for.predicting, number.of.features.to.select)
    
    print(paste("Testing all combination with length: ", number.of.features.to.select, " out of ", combinations.length , sep = ""))
    for (possible.combination.idx in 1:dim(all.possible.combinations)[2]) {
      
      print(paste("Testing specifing combination with idx ", possible.combination.idx, " out of ", dim(all.possible.combinations)[2] , sep = ""))
      possible.combination <- all.possible.combinations[,possible.combination.idx]
      
      # always keep target inside dataframe
      features.for.predicting.current.iter <- c(possible.combination, 'target')
      
      CV.error <- mean(replicate(repeat.k.fold, cross.validate.model(data.for.training, build.model, features.for.predicting.current.iter, predict.type = predict.type)))
      
      key.CV.error.res <- paste("CV.error_", number.of.features.to.select, "_", possible.combination.idx, sep = "")
      key.subset.res <- paste("CV.subset_features_", number.of.features.to.select, "_", possible.combination.idx, sep = "")
      
      result[[key.CV.error.res]] <- CV.error
      result[[key.subset.res]] <- possible.combination
      
      if (CV.error < lowest.CV.error.seen) {
        lowest.CV.error.seen <- CV.error
        best.subset.features <- possible.combination
      }
    }
  }
  
  result[["lowest.CV.error.seen"]] <- lowest.CV.error.seen
  result[["best.subset.features"]] <- best.subset.features
  
  return(result)
}

#' Determine best subset of errors by trying to compute cross-validation errors 
#' and greedily removing feature which causes best improvement on CV-error after removal.
#' @param data.for.training data that will be used for building model and predictin test error
#' @param features.for.predicting vector of strings containing names of features that can be used for building model
#' @param build.model function which builds model
#'     function Args:
#'         - data.for.training -- dataframe containing data for training
#'         - features.for.predicting - vector of strings containing names of features that can be used for building model
#'     Returns:
#          - list which contains built model under key "model" (e.g. result[['model]])
#' @return information about best subset of features in form of hash dictionary
determine.best.subset.of.features.using.greedy <- function(data.for.training, features.for.predicting, build.model, predict.type = "response", repeat.k.fold = 1) {
  result <- hash()
  lowest.CV.error.seen <- mean(replicate(repeat.k.fold, cross.validate.model(data.for.training, build.model, c(features.for.predicting, "target"), predict.type = predict.type)))
  
  result[["CV_errors_seen"]] = c(lowest.CV.error.seen)
  result[["CV_errors_seen_feature_subset"]] = c(features.for.predicting)
  
  
  best.subset.features <- features.for.predicting
  best.features <- features.for.predicting
  
  iter <- 0
  improvement.was.made <- TRUE
  while (improvement.was.made) {
    improvement.was.made <- FALSE
    iter <- iter + 1
    
    nested_iter <- 0
    for (feature in best.subset.features) {
      nested_iter <- nested_iter + 1
      features.to.try <- best.subset.features[!best.subset.features == feature]
      features.for.predicting.current.iter <- c(features.to.try, 'target')
      CV.error <- mean(replicate(repeat.k.fold, cross.validate.model(data.for.training, build.model, features.for.predicting.current.iter, predict.type = predict.type, eliminate.target.from.predict = eliminate.target.from.predict)))
      
      result[[paste("CV_error_iter_", iter, "_", nested_iter, sep = "")]] <- CV.error
      result[[paste("feature_subset_iter_", iter, "_", nested_iter, sep = "")]] <- features.to.try
      
      result[["CV_errors_seen"]] <- c(result[["CV_errors_seen"]], CV.error)
      result[["CV_errors_seen_feature_subset"]] <- c(result[["CV_errors_seen_feature_subset"]], features.to.try)
      
      if (CV.error < lowest.CV.error.seen) {
        lowest.CV.error.seen <- CV.error
        best.features <- features.to.try
        improvement.was.made <- TRUE
      }
    }
    
    best.subset.features <- best.features
  }
  
  result[["lowest.CV.error.seen"]] <- lowest.CV.error.seen
  result[["best.subset.features"]] <- best.subset.features
  
  return(result)
}



