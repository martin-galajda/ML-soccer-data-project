library(e1071)

### Prepare data and runs the model
run.svregression <- function () {
  
  ## Note: If matches.merged.all.features is not already loaded to your environment, uncomment the following line
  matches.merged.all.features = load.matches.with.all.features.for.match.result()
  
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
  
  matches.for.training.home <- matches.merged.all.features[,features.for.keeping]
  matches.for.training.home$target <- matches.merged.all.features$home_team_goal
  
  matches.for.training.away <- matches.merged.all.features[,features.for.keeping]
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
  prediction.home <- make.svregression.model (matches.for.training.home, features.for.predicting, "last.season")
  
  ## PREDICT AWAY
  prediction.away <- make.svregression.model (matches.for.training.away, features.for.predicting, "last.season")
  
  ## TUNE HOME
  #prediction.home <- tune.svregression.model (matches.for.training.home, features.for.predicting, "last.season")
  
  ## TUNE AWAY
  #prediction.away <- tune.svregression.model (matches.for.training.away, features.for.predicting, "last.season")
  
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
  
  return( prediction )
}

### This function creates the model and predicts goals using the optimal parameters (found in previous model tuning)
### depending on the column copied into "target" this function predicts the goals for the home or away team
make.svregression.model <- function(matches, features.for.predicting, test.method) {
  
  ## spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches <- matches[,features.for.predicting]
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  ## create and train the model
  model.svr <- svm(target ~., data = matches.train, epsilon = 0.6, cost=2)
 
  ## predict goals
  predicted.goals <- predict(model.svr, data = matches.test)
  
  ## calculate error end accuracy
  mse.svr <-  mean((matches.test$target - predicted.goals)^2)
  nrmse.svr <-  (mean((matches.test$target - predicted.goals)^2)/var(matches$target))^0.5
  accuracy.svr <- mean(matches.test$target == round(predicted.goals))
  
  print(paste0("mse:       ", mse.svr))
  print(paste0("nrmse:     ", nrmse.svr))
  print(paste0("accuracy:  ", accuracy.svr))
  
  result = vector(mode="list")
  result[["model"]] <- model.svr
  result[["predictions"]] <- predicted.goals
  result[["mse"]] <- mse.svr
  result[["nrmse"]] <- nrmse.svr
  result[["accuracy"]] <- accuracy.svr
   
  return (result) 
}

### This function tunes the models parameter (cost and epsilon) and runs the model afterwards
### depending on the column copied into "target" this function predicts the goals for the home or away team
### The tuning of the model might take a long time (depending on the parameters it took me 10 to 30 hours)
tune.svregression.model <- function(matches, features.for.predicting, test.method) {

  matches <- matches[,features.for.predicting]
  
  ## spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  ## tuning of the model
  tuneResult <- tune(svm, target ~., data = matches.train,
                     ranges = list(epsilon = seq(0,1,0.2), cost = 2^(1:5))
  )
  
  print(tuneResult)
  
  ## select and run the  best model found before
  model.svr <- tuneResult$best.model
  
  ## predict goals
  predicted.goals <- predict(model.svr, data = matches.test) 
  
  ## calculate error end accuracy
  mse.svr <- mean((matches.test$target - predicted.goals)^2)
  nrmse.svr <-  (mean((matches.test$target - predicted.goals)^2)/var(matches$target))^0.5
  accuracy.svr <- mean(matches.test$target == round(predicted.goals))
  
  print(paste0("mse:       ", mse.svr))
  print(paste0("nrmse:     ", nrmse.svr))
  print(paste0("accuracy:  ", accuracy.svr))
  
  result = vector(mode="list")
  result[["model"]] <- model.svr
  result[["predictions"]] <- predicted.goals
  result[["mse"]] <- mse.svr
  result[["nrmse"]] <- nrmse.svr
  result[["accuracy"]] <- accuracy.svr
  
  return (result) 
}
