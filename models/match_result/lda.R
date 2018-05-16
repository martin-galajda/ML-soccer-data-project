library(MASS)

make.lda.model <- function(matches) {
  result.mapping = vector(mode="list", length=length(levels(matches$result)))
  for (i in 1:length(levels(matches$result))) {
    result.mapping[[i]] = levels(matches$result)[i]
  }
  
  model.lda <- lda(result ~ strength.home.team + strength.away.team, data = matches)
  predictions.lda <- predict(model.lda, type="response")
  predictions <- predictions.lda$posterior
  
  predicted.results <- c()
  for (i in 1:dim(predictions)[1]) {
    # get max probability for predicting result
    predicted.value.index <- which(predictions[i,] == max(predictions[i,]))[1]
    
    # get string value for predicting result (e.g. "Home win")
    predicted.result <- result.mapping[[predicted.value.index]]
    
    # append the result to the results list
    predicted.results <- c(predicted.results, predicted.result)
  }
  prediction.accuracy.lda <- mean(matches$result == predicted.results)

  result = vector(mode="list", length=3)
  result[["model"]] <- model.lda
  result[["predictions"]] <- predicted.results
  result[["accuracy"]] <- prediction.accuracy.lda
  
  return(result)
}

# Example usage (requires strenght.home.team and strenght.away.team computed inside matches_csv)
# result <- make.lda.model(matches_csv)