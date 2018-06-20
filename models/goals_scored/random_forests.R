# randomForest metrics - complete script

source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')

# libraries

library(randomForest)

# general parameters

tuning.run = TRUE # run tuning to find best mtry
nTree = 500

# specific parameters

mtry = 2          # number of features used in splits (overwritten if tuning.run = TRUE)

# load data

matches = load.matches.with.all.features.for.match.result()
features.train = c("attack.strength.home.team","attack.strength.away.team",
                   "win.ratio.home.team","win.ratio.home.team.playing.home",
                   "win.ratio.home.team.playing.away","win.ratio.away.team",
                   "win.ratio.away.team.playing.home","win.ratio.away.team.playing.away")
index.te = which(matches$season == "2015/2016")
matches.train = matches[-index.te,]
matches.test = matches[index.te,]

# tuning parameters

if (tuning.run == T) {
  res = tuneRF(as.matrix(matches.train[,features.train]),matches.train$home_team_goal,plot=FALSE,ntreeTry=nTree)
  print(res)
  mtry = res[as.numeric(which.min(res[1:3,2])),1]
  print(mtry)
}

# train models

home.model = randomForest(as.matrix(matches.train[,features.train]),matches.train$home_team_goal,mtry=mtry,ntree=nTree)
away.model = randomForest(as.matrix(matches.train[,features.train]),matches.train$away_team_goal,mtry=mtry,ntree=nTree)

# predict

home.predictions = predict(home.model,as.matrix(matches.test[,features.train]),type="response")
away.predictions = predict(away.model,as.matrix(matches.test[,features.train]),type="response")

# mse

home.mse = mean((matches.test$home_team_goal - home.predictions)^2)
away.mse = mean((matches.test$away_team_goal - away.predictions)^2)
avrg.mse = (home.mse+away.mse)/2

# nrmse

home.rmse = (home.mse/var(matches$home_team_goal))^0.5
away.rmse = (away.mse/var(matches$away_team_goal))^0.5
avrg.rmse = (home.rmse+away.rmse)/2

# distribution

home.exact = round(home.predictions)
away.exact = round(away.predictions)
d = data.frame(table(c(home.exact,away.exact)))
d$dist = d$Freq/sum(d$Freq) ######################## use this value for goals_distribution

# accuracy 

home.acc = mean(matches.test$home_team_goal == home.exact)
away.acc = mean(matches.test$away_team_goal == away.exact)
avrg.acc = (home.acc+away.acc)/2
