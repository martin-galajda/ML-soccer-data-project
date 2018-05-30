load.matches.win.with.ratio.from.prev.10.matches <- function() {
  # This will precompute features of win ratios from prev 10 matches to csv
  # (if it was not computed yet)
  # and save it to "matches_with_win_ratio_from_prev_10_matches.csv" file inside data/ dir
  source('./feature_extraction/precompute_features/precompute_win_ratios_prev_10.R')
  
  matches <- read.csv('./data/matches_with_win_ratio_from_prev_10_matches.csv')
  
  return(matches)
}