library(e1071)
library(BBmisc)

### Prepare data and runs the model
run.knn.regression <- function () {
  
  ## Note: If matches.merged.all.features is not already loaded to your environment, uncomment the following line
  ## matches.merged.all.features = load.matches.with.all.features.for.match.result()
  
  ## Select features for predicting
  features.for.keeping <- c(
    'result',
    'season',
    'attack.strength.home.team',
    'attack.strength.away.team',
    'win.ratio.home.team',
    'win.ratio.away.team',
    'win.ratio.home.team.playing.home',
    'win.ratio.home.team.playing.away',
    'win.ratio.away.team.playing.home',
    'win.ratio.away.team.playing.away'
  )
  
  ## select from dataframe only columns relevant for predicting
  features.for.predicting <- features.for.keeping[3:length(features.for.keeping)]
  
  #normalize data and prepare home and away datasets
  matches.for.training.home <- normalize(matches.merged.all.features[,features.for.keeping])
  matches.for.training.home$target <- matches.merged.all.features$home_team_goal
  
  matches.for.training.away <- normalize(matches.merged.all.features[,features.for.keeping])
  matches.for.training.away$target <- matches.merged.all.features$away_team_goal
  
  features.for.predicting <- c(
    'target', # this needs to always be inside dataframe
    'attack.strength.home.team',
    'attack.strength.away.team',
    'win.ratio.home.team',
    'win.ratio.away.team',
    'win.ratio.home.team.playing.home',
    'win.ratio.away.team.playing.away'
  )
  
  ## Ensure reproducable results
  set.seed(101)
  
  ## Note: Comment and uncomment the following lines depending on whether predicting or tuning is required and
  ##       whether home or away goals shall be predicted
  
  ## PREDICT HOME
  prediction.home <- make.knn.model (matches.for.training.home, features.for.predicting, "last.season", 120)
  
  ## PREDICT AWAY
  prediction.away <- make.knn.model (matches.for.training.away, features.for.predicting, "last.season", 163)
  
  ## TUNE HOME
  #prediction.home <- tune.knn.model (matches.for.training.home, features.for.predicting, "last.season", 1:300)
  
  ## TUNE AWAY
  #prediction.away <- tune.knn.model (matches.for.training.away, features.for.predicting, "last.season", 1:300)

  prediction = vector(mode="list")
  prediction[["model.home"]] <- prediction.home[["model"]] 
  prediction[["model.away"]] <- prediction.home[["model"]] 
  prediction[["predictions.home"]] <- prediction.home[["predictions"]]
  prediction[["predictions.away"]] <- prediction.away[["predictions"]] 
  prediction[["mse.avg"]] <- (prediction.home[["mse"]]+prediction.away[["mse"]])/2
  prediction[["nrmse.avg"]] <- (prediction.home[["nrmse"]]+prediction.away[["nrmse"]])/2
  prediction[["accuracy.avg"]] <- (prediction.home[["accuracy"]]+prediction.away[["accuracy"]])/2
  
  print(paste0("mse (avg):       ", prediction[["mse.avg"]]))
  print(paste0("nrmse (avg):     ", prediction[["nrmse.avg"]]))
  print(paste0("accuracy (avg):  ", prediction[["accuracy.avg"]]))
  
  return(prediction)
}

### This function creates the model and predicts goals using the optimal parameter k (found in model tuning)
### depending on the column copied into "target" this function predicts the goals for the home or away team
### Parameters:
###     matches     - pass the dataset for training and testing
###     test.method - pass either "last.season" or percentage for test data (e.g. 0.1)
###     k           - pass an integer for k; default is 1
### Return:
###     The function returns a "result" object containing the model, predicted goals, nrmse and accuracy  
make.knn.model <- function(matches, features.for.predicting, test.method, k = 1) {
  
  matches <- matches[,features.for.predicting]
  
  ## spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  ## create and train the model
  pred_001 = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = k)
  predicted.goals <- pred_001$pred
  
  ## calculate nrmse and accuracy
  mse.knn <-  mean((matches.test$target - predicted.goals)^2)
  nrmse.knn <-  (mean((matches.test$target - predicted.goals)^2)/var(matches$target))^0.5
  accuracy.knn <- mean(matches.test$target == round(predicted.goals))
  
  print(paste0("mse:       ", mse.knn))
  print(paste0("nrmse:     ", nrmse.knn))
  print(paste0("accuracy:  ", accuracy.knn))
  
  result = vector(mode="list")
  result[["model"]] <- pred_001
  result[["predictions"]] <- predicted.goals
  result[["mse"]] <- mse.knn
  result[["nrmse"]] <- nrmse.knn
  result[["accuracy"]] <- accuracy.knn
  
  return (result) 
}

### This function tunes the models parameter k and runs the model afterwards
### depending on the column copied into "target" this function predicts the goals for the home or away team
### Parameters:
###     matches     - pass the dataset for training and testing
###     test.method - pass either "last.season" or percentage for test data (e.g. 0.1)
###     k.values    - pass either an integer or a range (e.g. 1:50); default is 1
### Return:
###     Additional to the values retunred by make.knn.model this function retunrs the best k found
tune.knn.model <- function(matches, features.for.predicting, test.method, k.values) {
  
  matches <- matches[,features.for.predicting]
  
  ## spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  ## create and train the model
  k.errors <- c()
  
  for(k in k.values){
    pred = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = k)
    k.errors[k] <-  (mean((matches.test$target - pred$pred)^2)/var(matches.test$target))^0.5
    #k.errors[k] <-  mean(matches.test$target == round(pred$pred))
  }
  
  best.k <- which.min(k.errors)
  print(paste0("best k:    ", best.k))
  
  ## create and run model with best k found before
  pred = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = best.k)
  predicted.goals <- pred$pred
  
  ## calculate nrmse and accuracy
  mse.knn <-  mean((matches.test$target - predicted.goals)^2)
  nrmse.knn <-  (mean((matches.test$target - predicted.goals)^2)/var(matches.test$target))^0.5
  accuracy.knn <- mean(matches.test$target == round(predicted.goals))
  
  print(paste0("mse:       ", mse.knn))  
  print(paste0("nrmse:     ", nrmse.knn))
  print(paste0("accuracy:  ", accuracy.knn))
  
  result = vector(mode="list")
  result[["model"]] <- pred
  result[["predictions"]] <- predicted.goals
  result[["mse"]] <- mse.knn
  result[["nrmse"]] <- nrmse.knn
  result[["accuracy"]] <- accuracy.knn
  result[["best_k"]] <- best.k
  
  return (result) 
}
