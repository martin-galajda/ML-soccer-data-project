library(TunePareto) # for generateCVRuns()

cross.validate.model <- function(data, build.model, build.model.params, k = 10) {
  CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)

  cv.results <- matrix (rep(0, 4*k),nrow=k)
  colnames (cv.results) <- c("k", "fold", "TR error", "VA error")

  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k

  for (j in 1:k) {
    # get validation indices for data
    validation.indices <- unlist(CV.folds[[1]][[j]])

    # train on TR data
    # my.lda.TR <- lda(target ~ X1 + X2, data = data[-va,], prior=priors, CV=FALSE)
    model.training <- build.model(data[-validation.indices, ], build.model.params)
    
    # predict training data
    pred.model.training <- predict(model.training$model)

    if (typeof(pred.model.training) == "list") {
      pred.model.training <- pred.model.training$class
    }
    
    table.result <- table(data[-validation.indices,]$target, pred.model.training)
    cv.results[j, "TR error"] <- 1 - sum(table.result[row(table.result)==col(table.result)])/sum(table.result)
    
    # predict validation data
    pred.cross.validation <- predict(model.training$model, newdata=data[validation.indices,])

    if (typeof(pred.cross.validation) == "list") {
      pred.cross.validation <- pred.cross.validation$class
    }
    
    table.result <- table(data[validation.indices,]$target, pred.cross.validation)
    cv.results[j, "VA error"] <- 1-sum(table.result[row(table.result)==col(table.result)])/sum(table.result)
    
    cv.results[j, "fold"] <- j

    print(paste("K = ", j, " out of ", k, " used for cross-validation.", sep = ""))
  }

  VA.error <- mean(cv.results[,"VA error"])
  return(VA.error)
}