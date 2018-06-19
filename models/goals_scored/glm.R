# glm metrics - complete script

source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')

# parameters

glm.family = "gaussian" # "poisson" or "gaussian"

# load data

matches = load.matches.with.all.features.for.match.result()
features.train = c("home_team_goal","away_team_goal",
                   "attack.strength.home.team","attack.strength.away.team",
                   "win.ratio.home.team","win.ratio.home.team.playing.home",
                   "win.ratio.home.team.playing.away","win.ratio.away.team",
                   "win.ratio.away.team.playing.home","win.ratio.away.team.playing.away")
index.te = which(matches$season == "2015/2016")
matches.train = matches[-index.te,]
matches.test = matches[index.te,]

# train models

home.model = glm(home_team_goal ~ . - away_team_goal, family=glm.family, data=matches.train[,features.train])
away.model = glm(away_team_goal ~ . - home_team_goal, family=glm.family, data=matches.train[,features.train])

# simplify models

home.model = step(home.model)
away.model = step(away.model)

# predict

home.predictions = predict(home.model,newdata=matches.test,type="response")
away.predictions = predict(away.model,newdata=matches.test,type="response")

# mse

home.mse = mean((matches.test$home_team_goal - home.predictions)^2)
away.mse = mean((matches.test$away_team_goal - away.predictions)^2)
avrg.mse = (home.mse+away.mse)/2

# nrmse

home.rmse = (home.mse/var(matches$home_team_goal))^0.5
away.rmse = (away.mse/var(matches$away_team_goal))^0.5
avrg.rmse = (home.rmse+away.rmse)/2

# distribution

if (glm.family == "normal") {
  home.exact = round(home.predictions)
  away.exact = round(away.predictions)
} else {
  home.exact = c()
  away.exact = c()
  for (i in 1:nrow(matches.test)) {
    home.exact = c(home.exact,which(dpois(0:10,home.predictions[i]) == max(dpois(0:10,home.predictions[i])))-1)
    away.exact = c(away.exact,which(dpois(0:10,away.predictions[i]) == max(dpois(0:10,away.predictions[i])))-1)
  }
}
d = data.frame(table(c(home.exact,away.exact)))
d$dist = d$Freq/sum(d$Freq) ######################## use this value for goals_distribution

# accuracy

home.acc = mean(matches.test$home_team_goal == home.exact)
away.acc = mean(matches.test$away_team_goal == away.exact)
avrg.acc = (home.acc+away.acc)/2
