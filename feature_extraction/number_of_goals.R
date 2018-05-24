# Compute the number of goals for a given team and period of time.
# Expected parameters:
# - team, defining the team by it's name
# - number.matches, defining the number of matches to take into account
# - home.away.selector, (optional) expects "home" or "away" if only home/ away games should be considered
# Returns the number of goals.

# matches_csv = read.csv('./data/db_matches.csv')

compute_number_of_goals <- function(matches_csv, team, number.matches, home.away.selector = NULL) {
  
  # select relevant rows
  if(is.character(home.away.selector)){
    if(home.away.selector == "home")
      filtered.matches <- matches_csv[as.character(matches_csv$home_team) == team, ]
    else if (home.away.selector == "away")
      filtered.matches <- matches_csv[as.character(matches_csv$away_team) == team, ]
  }
  else{
    filtered.matches <- subset(matches_csv, as.character(matches_csv$home_team) == team | as.character(matches_csv$away_team) == team,)
  }
   
  # order by date
  filtered.matches <- filtered.matches[order(filtered.matches$date),]  
  
  # select newest n rows
  filtered.matches <- tail( filtered.matches, number.matches)
  
  # select home matches
  filtered.matches.home <- filtered.matches[as.character(filtered.matches$home_team) == team, ]
  
  # select away matches
  filtered.matches.away <- filtered.matches[as.character(filtered.matches$away_team) == team, ]

  # calculate sum of goals
  result <- sum(filtered.matches.home$home_team_goal) + sum(filtered.matches.away$away_team_goal)
  
  # return results
  return(result)
}

# Example usage:
# compute_number_of_goals(matches_csv, "Liverpool", 10)
# or
# compute_number_of_goals(matches_csv, "Liverpool", 10, "home")
# or
# compute_number_of_goals(matches_csv, "Liverpool", 10, "away")