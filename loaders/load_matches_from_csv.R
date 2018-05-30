
load.matches.from.csv <- function() {
  matches_csv = read.csv('./data/db_matches.csv')

  # Add results to the csv
  matches_csv$result <- rep(NA, dim(matches_csv)[1])
  matches_csv$result[matches_csv$home_team_goal > matches_csv$away_team_goal] = "Home win"
  matches_csv$result[matches_csv$home_team_goal < matches_csv$away_team_goal] = "Away win"
  matches_csv$result[matches_csv$home_team_goal == matches_csv$away_team_goal] = "Tie"
  matches_csv$result <- as.factor(matches_csv$result)
  
  matches_csv$date <- as.Date(matches_csv$date)
  
  matches_csv <- matches_csv[order(matches_csv$date), ]
  matches_csv$id <- 1:dim(matches_csv)[1]
  
  return(matches_csv)
}
