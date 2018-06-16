source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')

# parameters
test.method = 0.2 # "last.season" or test random proportion (e.g. 20%)

# load data (TODO: load differrent features than match.result (?))
matches = load.matches.with.all.features.for.match.result()
# feature selection (TODO: do some kind of crossvalidation (?))
features.train = c("home_team_goal","away_team_goal",
                   "attack.strength.home.team","attack.strength.away.team",
                   "win.ratio.home.team","win.ratio.home.team.playing.home",
                   "win.ratio.home.team.playing.away","win.ratio.away.team",
                   "win.ratio.away.team.playing.home","win.ratio.away.team.playing.away")
# train-test split
index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
                                             sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
matches.train = matches[-index.te,]
matches.test = matches[index.te,]
# train models
home.model = glm(home_team_goal ~ . - away_team_goal, family=poisson(), data=matches.train[,features.train])
away.model = glm(away_team_goal ~ . - home_team_goal, family=poisson(), data=matches.train[,features.train])
# test: predict & obtain metrics
home.predictions = predict(home.model,newdata=matches.test,type="response")
away.predictions = predict(away.model,newdata=matches.test,type="response")
home.nrmse = (mean((matches.test$home_team_goal - home.predictions)^2)/var(matches$home_team_goal))^0.5
away.nrmse = (mean((matches.test$away_team_goal - away.predictions)^2)/var(matches$away_team_goal))^0.5
home.accuracy = mean(matches.test$home_team_goal == round(home.predictions))
away.accuracy = mean(matches.test$away_team_goal == round(away.predictions))
