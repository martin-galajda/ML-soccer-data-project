# Function for computing attack strength for given match for team playing at home and playing away
# Inspired by https://www.pinnacle.com/en/betting-articles/Soccer/how-to-calculate-poisson-distribution/MD62MLXUMKMXZ6A8
compute.attack.strengths <- function(matches, match) {
  team <- match$home_team
  season <- match$season
  away.team <- match$away_team
  
  # Filter matches
  matches.from.the.past <- matches[matches$season == season, ]
  past.matches.home.team.playing.home <- matches.from.the.past[matches.from.the.past$home_team == team,]
  past.matches.away.team.playing.away <- matches.from.the.past[matches.from.the.past$away_team == away.team,]
  
  # Compute attack score for home team
  goal.ratio.playing.home.overal <- sum(matches.from.the.past$home_team_goal) / dim(matches.from.the.past)[1]
  home.team.goal.ratio.playing.home <- sum(past.matches.home.team.playing.home$home_team_goal) / dim(past.matches.home.team.playing.home)[1]
  attack.score.home.team <- home.team.goal.ratio.playing.home / goal.ratio.playing.home.overal
  
  # Compute attack score for away team
  goal.ratio.playing.away.overal <- sum(matches.from.the.past$away_team_goal) / dim(matches.from.the.past)[1]
  away.team.goal.ratio.playing.away <- sum(past.matches.away.team.playing.away$away_team_goal) / dim(past.matches.away.team.playing.away)[1]
  attack.score.away.team <- away.team.goal.ratio.playing.away / goal.ratio.playing.away.overal
  
  # Compute defense score for home team
  goal.condeded.ratio.playing.home.overal <- sum(matches.from.the.past$away_team_goal) / dim(matches.from.the.past)[1]
  goal.condeded.ratio.home.team.playing.home <- sum(past.matches.home.team.playing.home$away_team_goal) / dim(past.matches.home.team.playing.home)[1]
  defense.score.home.team <- goal.condeded.ratio.home.team.playing.home / goal.condeded.ratio.playing.home.overal
  
  # Compute defense score for away team
  goal.condeded.ratio.playing.away.overal <- sum(matches.from.the.past$home_team_goal) / dim(matches.from.the.past)[1]
  goal.condeded.ratio.away.team.playing.away <- sum(past.matches.away.team.playing.away$home_team_goal) / dim(past.matches.away.team.playing.away)[1]
  defense.score.away.team <- goal.condeded.ratio.away.team.playing.away / goal.condeded.ratio.playing.away.overal
  
  # Put it together and compute attack strengths of the teams
  attack.strength.home.team <- defense.score.away.team * attack.score.home.team * goal.ratio.playing.home.overal
  attack.strength.away.team <- defense.score.home.team * attack.score.away.team * goal.ratio.playing.away.overal
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
  away.team <- match$away_team
  
  # Filter matches from the past
  matches.from.the.past <- matches[matches$date < match$date, ]
  past.matches.home.team.playing.home <- matches.from.the.past[matches.from.the.past$home_team == team,]
  past.matches.away.team.playing.away <- matches.from.the.past[matches.from.the.past$away_team == away.team,]
  
  # Compute attack score for home team
  goal.ratio.playing.home.overal <- sum(matches.from.the.past$home_team_goal) / dim(matches.from.the.past)[1]
  home.team.goal.ratio.playing.home <- sum(past.matches.home.team.playing.home$home_team_goal) / dim(past.matches.home.team.playing.home)[1]
  attack.score.home.team <- home.team.goal.ratio.playing.home / goal.ratio.playing.home.overal
  
  # Compute attack score for away team
  goal.ratio.playing.away.overal <- sum(matches.from.the.past$away_team_goal) / dim(matches.from.the.past)[1]
  away.team.goal.ratio.playing.away <- sum(past.matches.away.team.playing.away$away_team_goal) / dim(past.matches.away.team.playing.away)[1]
  attack.score.away.team <- away.team.goal.ratio.playing.away / goal.ratio.playing.away.overal
  
  # Compute defense score for home team
  goal.condeded.ratio.playing.home.overal <- sum(matches.from.the.past$away_team_goal) / dim(matches.from.the.past)[1]
  goal.condeded.ratio.home.team.playing.home <- sum(past.matches.home.team.playing.home$away_team_goal) / dim(past.matches.home.team.playing.home)[1]
  defense.score.home.team <- goal.condeded.ratio.home.team.playing.home / goal.condeded.ratio.playing.home.overal
  
  # Compute defense score for away team
  goal.condeded.ratio.playing.away.overal <- sum(matches.from.the.past$home_team_goal) / dim(matches.from.the.past)[1]
  goal.condeded.ratio.away.team.playing.away <- sum(past.matches.away.team.playing.away$home_team_goal) / dim(past.matches.away.team.playing.away)[1]
  defense.score.away.team <- goal.condeded.ratio.away.team.playing.away / goal.condeded.ratio.playing.away.overal
  
  # Put it together and compute attack strengths of the teams
  attack.strength.home.team <- defense.score.away.team * attack.score.home.team * goal.ratio.playing.home.overal
  attack.strength.away.team <- defense.score.home.team * attack.score.away.team * goal.ratio.playing.away.overal
  strengths <- data.frame(attack.strength.home.team, attack.strength.away.team)
  
  return(strengths)
}

# Example usage (attention, takes like 1 minute to compute):
# enhanced.matches <- attach.attack.strength.ratios(matches_csv)