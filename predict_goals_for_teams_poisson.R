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

# Function which uses attack strengths ratios and Poisson distribution
# for predicting how many goals will each team score
predict.goals.for.teams <- function(matches, match) {
  strengths <- compute.attack.strengths(matches, match)
  
  home.team.goal.probs <- c()
  away.team.goal.probs <- c()
  for (i in 0:5) {
    home.team.goal.prob <- dpois(i, lambda=strengths$attack.strength.home.team)
    home.team.goal.probs <- c(home.team.goal.probs, home.team.goal.prob)
    
    away.team.goal.prob <- dpois(i, lambda=strengths$attack.strength.away.team)
    away.team.goal.probs <- c(away.team.goal.probs, away.team.goal.prob)
  }

  probs <- rbind(home.team.goal.probs, away.team.goal.probs)
  home.team.goals.predicted <- which(max(probs[1, ]) == probs[1, ])
  away.team.goals.predicted <- which(max(probs[2, ]) == probs[2, ])
  
  goals.predicted <- data.frame(home.team.goals.predicted, away.team.goals.predicted)
  
  return(goals.predicted)
}

# Sample usage:
matches_csv = read.csv('./data/db_matches.csv')
sample.match <- matches_csv[1,]
strengths <- compute.attack.strengths(matches_csv, sample.match)
predicted.goals <- predict.goals.for.teams(matches_csv, sample.match)

