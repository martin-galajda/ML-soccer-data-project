predict.simple.poisson <- function(matches) {
  for (i in 0:5) {
    matches[,paste0('pred.home.goals.',i)] = dpois(i, lambda=matches$attack.strength.home.team)
    matches[,paste0('pred.away.goals.',i)] = dpois(i, lambda=matches$attack.strength.away.team)
  }
  return(matches)
}
