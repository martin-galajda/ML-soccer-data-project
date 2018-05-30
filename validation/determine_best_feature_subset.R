library(hash)
## hash-2.2.6 provided by Decision Patterns
source('./validation/cross_validate.R')

determine.best.subset.of.features <- function(data.for.training, features.for.predicting, build.model) {
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
      
      CV.error <- cross.validate.model(matches.for.training, build.model, features.for.predicting.current.iter)
      
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