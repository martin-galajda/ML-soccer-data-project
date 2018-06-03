# Compute the number of goals for a given team in a given number of matches prior a given match.
# Expected parameters:
# - matches_csv         expects the data
# - sample.match        expects a match (only matches prior this match are considered)
# - number.matches      expects the number of matches to take into account
# - home.away.selector  (optional) expects "home" or "away" if only home/ away games should be considered
# Returns the number of goals.

compute_number_of_goals <- function (matches_csv, sample.match, number.matches, home.away.selector = NULL) {
  #match <- matches_csv[matches_csv$X == match.id,]
  match <- sample.match
  
  # select relevant rows
  if(is.character(home.away.selector)){
    if(home.away.selector == "home")
      filtered.matches <- matches_csv[as.character(matches_csv$home_team) == match$home_team, ]
    else if (home.away.selector == "away")
      filtered.matches <- matches_csv[as.character(matches_csv$away_team) == match$home_team, ]
  }
  else{
    filtered.matches <- subset(matches_csv, as.character(matches_csv$home_team) == match$home_team | as.character(matches_csv$away_team) == match$home_team)
  }
  
  # order by date
  filtered.matches <- filtered.matches[order(filtered.matches$date),]
  
  # select only older games
  filtered.matches <- filtered.matches[as.Date(filtered.matches$date) < as.Date(match$date),]
  
  # select newest n rows
  filtered.matches <- tail( filtered.matches, number.matches)
  
  # select home matches
  filtered.matches.home <- filtered.matches[as.character(filtered.matches$home_team) == match$home_team, ]

  # select away matches
  filtered.matches.away <- filtered.matches[as.character(filtered.matches$away_team) == match$home_team, ]

  # calculate sum of goals
  result <- sum(filtered.matches.home$home_team_goal) + sum(filtered.matches.away$away_team_goal)
  
  return(result) 
}

# Example usage:
# compute_number_of_goals(matches_csv, 22473, 5)
# or
# compute_number_of_goals(matches_csv, 22473, 5, "home")
# or
# compute_number_of_goals(matches_csv, 22473, 5, "away")
