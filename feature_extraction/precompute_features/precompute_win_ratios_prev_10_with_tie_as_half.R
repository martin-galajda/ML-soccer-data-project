source('./feature_extraction/precompute_features/precompute_win_ratios.R')

# This will precompute win ratios always with respect to last 10 matches 
# and save them in the data/ directory

# create file only when doesnt exist
if (!file.exists('./data/matches_with_win_ratio_from_prev_10_matches.csv')) {
  precompute.win.ratios.from.prev.matches(num.of.prev.matches.for.ratio = 10, with.tie.as.half.win = TRUE)
}
