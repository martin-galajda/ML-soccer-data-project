load.matches.with.attack.ratios <- function() {
  # This will compute attack strength ratios (if there were not already saved to csv)
  # and save them to the csv file
  source('./feature_extraction/precompute_features/precompute_attack_strength_ratios.R')
  
  matches <- read.csv('./data/matches_with_attack_strength_from_prev_matches.csv')
  
  return(matches)
}