source('./feature_extraction/win_ratios.R')
library(ggplot2)

matches_csv = read.csv('./data/db_matches.csv')
players_csv = read.csv('./data/db_players.csv')

matches_csv$date <- as.Date(matches_csv$date)

# Build dictionary mapping team name to simplest win ratio
team.dictionary = vector(mode="list", length=length(levels(matches_csv$home_team)))
for (i in 1:length(levels(matches_csv$home_team))) {
  team <- levels(matches_csv$home_team)[i]
  team.dictionary[[i]] <- compute_ratios(matches_csv, start.date = as.Date("1900-10-15"), end.date = as.Date("2018-01-14"), team = team)
}

names(team.dictionary) <- levels(matches_csv$home_team)

matches_csv$win.ratio.home.team <- rep(NA, dim(matches_csv)[1])

for (i in 1:dim(matches_csv)[1]) {
  team <- matches_csv$home_team[i]
  team.info <- team.dictionary[[team]]
  matches_csv$win.ratio.home.team[i] <- team.info$win.ratio 
}

max.prediction.accuracy <- 0
max.margin <- 0
predictions.accuracies.for.best.margin <- c()


for (margin.for.tie in seq(0, 30, 0.5)) {
  prediction.accuracies <- c()
  
  for (min.win.ratio in seq(0, 1, 0.01)) {
    matches_csv$predicted.result <- rep(NA, dim(matches_csv)[1])
    matches_csv$predicted.result[matches_csv$win.ratio.home.team >= min.win.ratio] = "Home win"
    matches_csv$predicted.result[matches_csv$win.ratio.home.team > (min.win.ratio - margin.for.tie) & matches_csv$win.ratio.home.team < min.win.ratio] = "Tie"
    matches_csv$predicted.result[matches_csv$win.ratio.home.team <= (min.win.ratio - margin.for.tie) ] = "Away win"
    
    matches_csv$predicted.result <- as.factor(matches_csv$predicted.result)

    prediction.accuracy <- length(which(as.character(matches_csv$predicted.result) == as.character(matches_csv$result))) / dim(matches_csv)[1]
    prediction.accuracies <- c(prediction.accuracies, prediction.accuracy)
  }
  
  
  if (max(prediction.accuracies) > max.prediction.accuracy) {
    predictions.accuracies.for.best.margin <- prediction.accuracies
    max.prediction.accuracy <- max(prediction.accuracies)
    max.margin <- margin.for.tie
  }
}


ggplot() +
  geom_line(aes(x = seq(0,1,0.01), y = predictions.accuracies.for.best.margin)) +
  labs(x = "Min win ratio used for predicting", y = "Prediction accuracy")
