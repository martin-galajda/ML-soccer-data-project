source('./feature_extraction/attack_strength_ratio.R')

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
# sample.match <- matches_csv[1,]
# strengths <- compute.attack.strengths(matches_csv, sample.match)
# predicted.goals <- predict.goals.for.teams(matches_csv, sample.match)

