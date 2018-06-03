load.matches.with.win.ratio.from.prev.3.matches.with.tie.as.half <- function() {
  # This will precompute features of win ratios from prev 3 matches 
  # (if it was not computed yet)
  # and save it to "precompute_win_ratios_prev_3_with_tie_as_half.csv" inside data/ dir
  source('./feature_extraction/precompute_features/precompute_win_ratios_prev_3_with_tie_as_half.R')
  
  matches = read.csv('./data/matches_with_win_ratio_from_prev_3_matches_with_tie_as_half_win.csv')
  
  return(matches)
}