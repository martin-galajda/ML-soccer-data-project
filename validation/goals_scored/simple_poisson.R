# the model does not learn any parameter from the data, we don't need to
# cross_validate it and we can use the whole dataset as test

source('./loaders/load_matches_with_attack_ratios.R')
source('./models/goals_scored/simple_poisson.R')

matches = load.matches.with.attack.ratios()[,c('attack.strength.home.team','home_team_goal',
                                               'attack.strength.away.team','away_team_goal')]

# predict all matches

matches = predict.simple.poisson(matches)

# derivated predictions

## most probable outcome
matches$pred.home.exact.goal = apply(matches[,c(5,7,9,11,13,15)],  1, function(x) which(x == max(x))-1)
matches$pred.away.exact.goal = apply(matches[,c(6,8,10,12,14,16)], 1, function(x) which(x == max(x))-1)

## mean outcome
matches$pred.home.goal = apply(matches[,c(5,7,9,11,13,15)],  1, function(x) x %*% c(0,1,2,3,4,5))
matches$pred.away.goal = apply(matches[,c(6,8,10,12,14,16)], 1, function(x) x %*% c(0,1,2,3,4,5))

# validation metrics

## accuracy
matches$pred.home.acc = matches$home_team_goal == matches$pred.home.exact.goal
matches$pred.away.acc = matches$away_team_goal == matches$pred.away.exact.goal

## mae
mae.home = mean(abs(matches$home_team_goal - matches$pred.home.goal))
mae.away = mean(abs(matches$away_team_goal - matches$pred.away.goal))

## mse
mae.home = mean((matches$home_team_goal - matches$pred.home.goal)^2)
mae.away = mean((matches$away_team_goal - matches$pred.away.goal)^2)

## more... ?
