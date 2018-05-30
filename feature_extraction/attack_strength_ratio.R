# Function for computing attack strength for given match for team playing at home and playing away
# Inspired by https://www.pinnacle.com/en/betting-articles/Soccer/how-to-calculate-poisson-distribution/MD62MLXUMKMXZ6A8
compute.attack.strengths <- function(matches, match) {
  team <- match$home_team
  season <- match$season
  away.team <- match$away_team
  
  # Filter matches
  matches.for.given.season <- matches[matches$season == season, ]
  matches.for.home.team.at.home <- matches.for.given.season[matches.for.given.season$home_team == team,]
  matches.for.away.team.away <- matches.for.given.season[matches.for.given.season$away_team == away.team,]
  
  # Compute attack score for home team
  home.team.goal.ratio.league <- sum(matches.for.given.season$home_team_goal) / dim(matches.for.given.season)[1]
  home.team.goal.ratio.home.team <- sum(matches.for.home.team.at.home$home_team_goal) / dim(matches.for.home.team.at.home)[1]
  attack.score.home.team <- home.team.goal.ratio.home.team / home.team.goal.ratio.league
  
  # Compute attack score for away team
  away.team.goal.ratio.league <- sum(matches.for.given.season$away_team_goal) / dim(matches.for.given.season)[1]
  away.team.goal.ratio.for.away.team <- sum(matches.for.away.team.away$away_team_goal) / dim(matches.for.away.team.away)[1]
  attack.score.away.team <- away.team.goal.ratio.for.away.team / away.team.goal.ratio.league
  
  # Compute defense score for home team
  home.team.goal.condeded.ratio.league <- sum(matches.for.given.season$away_team_goal) / dim(matches.for.given.season)[1]
  home.team.goal.condeded.ratio.at.home <- sum(matches.for.home.team.at.home$away_team_goal) / dim(matches.for.home.team.at.home)[1]
  defense.score.home.team <- home.team.goal.condeded.ratio.at.home / home.team.goal.condeded.ratio.league
  
  # Compute defense score for away team
  away.team.goal.condeded.ratio.league <- sum(matches.for.given.season$home_team_goal) / dim(matches.for.given.season)[1]
  away.team.goal.condeded.ratio.away <- sum(matches.for.away.team.away$home_team_goal) / dim(matches.for.away.team.away)[1]
  defense.score.away.team <- away.team.goal.condeded.ratio.away / away.team.goal.condeded.ratio.league
  
  # Put it together and compute attack strengths of the teams
  attack.strength.home.team <- defense.score.away.team * attack.score.home.team * home.team.goal.ratio.league
  attack.strength.away.team <- defense.score.home.team * attack.score.away.team * away.team.goal.ratio.league
  strengths <- data.frame(attack.strength.home.team, attack.strength.away.team)
  
  return(strengths)
}

attach.attack.strength.ratios <- function(matches) {
  matches$strength.home.team <- rep(NA, dim(matches)[1])
  matches$strength.away.team <- rep(NA, dim(matches)[1])
  
  for (i in 1:dim(matches)[1]) {
    team <- matches$home_team[i]
    against.team <- matches$away_team[i]

    strengths.predicted <- compute.attack.strengths(matches, matches[i, ])
    matches$strength.home.team[i] <- strengths.predicted$attack.strength.home.team
    matches$strength.away.team[i] <- strengths.predicted$attack.strength.away.team
  }
  
  # Maybe don't mutate parameter that was passed but create new copy?
  return(matches)
}

compute.attack.strengths.for.match <- function(matches, match) {
  team <- match$home_team
  season <- match$season
  away.team <- match$away_team
  
  # Filter matches
  matches.for.given.season <- matches[matches$date < match$date, ]
  matches.for.home.team.at.home <- matches.for.given.season[matches.for.given.season$home_team == team,]
  matches.for.away.team.away <- matches.for.given.season[matches.for.given.season$away_team == away.team,]
  
  # Compute attack score for home team
  home.team.goal.ratio.league <- sum(matches.for.given.season$home_team_goal) / dim(matches.for.given.season)[1]
  home.team.goal.ratio.home.team <- sum(matches.for.home.team.at.home$home_team_goal) / dim(matches.for.home.team.at.home)[1]
  attack.score.home.team <- home.team.goal.ratio.home.team / home.team.goal.ratio.league
  
  # Compute attack score for away team
  away.team.goal.ratio.league <- sum(matches.for.given.season$away_team_goal) / dim(matches.for.given.season)[1]
  away.team.goal.ratio.for.away.team <- sum(matches.for.away.team.away$away_team_goal) / dim(matches.for.away.team.away)[1]
  attack.score.away.team <- away.team.goal.ratio.for.away.team / away.team.goal.ratio.league
  
  # Compute defense score for home team
  home.team.goal.condeded.ratio.league <- sum(matches.for.given.season$away_team_goal) / dim(matches.for.given.season)[1]
  home.team.goal.condeded.ratio.at.home <- sum(matches.for.home.team.at.home$away_team_goal) / dim(matches.for.home.team.at.home)[1]
  defense.score.home.team <- home.team.goal.condeded.ratio.at.home / home.team.goal.condeded.ratio.league
  
  # Compute defense score for away team
  away.team.goal.condeded.ratio.league <- sum(matches.for.given.season$home_team_goal) / dim(matches.for.given.season)[1]
  away.team.goal.condeded.ratio.away <- sum(matches.for.away.team.away$home_team_goal) / dim(matches.for.away.team.away)[1]
  defense.score.away.team <- away.team.goal.condeded.ratio.away / away.team.goal.condeded.ratio.league
  
  # Put it together and compute attack strengths of the teams
  attack.strength.home.team <- defense.score.away.team * attack.score.home.team * home.team.goal.ratio.league
  attack.strength.away.team <- defense.score.home.team * attack.score.away.team * away.team.goal.ratio.league
  strengths <- data.frame(attack.strength.home.team, attack.strength.away.team)
  
  return(strengths)
}

# Example usage (attention, takes like 1 minute to compute):
# enhanced.matches <- attach.attack.strength.ratios(matches_csv)