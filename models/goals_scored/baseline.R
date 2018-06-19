# baseline metrics - complete script

source('./loaders/load_matches_from_csv.R')

# load data

matches = load.matches.from.csv()
index.te = which(matches$season == "2015/2016")
matches.train = matches[-index.te,]
matches.test= matches[index.te,]
  
# baseline
  
home.mean = mean(matches.train$home_team_goal)
away.mean = mean(matches.train$away_team_goal)

# mse

home.mse = mean((matches.test$home_team_goal-home.mean)^2)
away.mse = mean((matches.test$away_team_goal-away.mean)^2)
avrg.mse = (home.mse+away.mse)/2

# nrmse

home.nrmse = (home.mse/var(matches$home_team_goal))^0.5
away.nrmse = (away.mse/var(matches$away_team_goal))^0.5
avrg.nrmse = (home.nrmse+away.nrmse)/2

# accuracy

home.acc = mean(matches$home_team_goal == 1)
away.acc = mean(matches$away_team_goal == 1)
avrg.acc = (home.acc+away.acc)/2
