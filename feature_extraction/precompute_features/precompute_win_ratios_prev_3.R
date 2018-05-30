source('./feature_extraction/precompute_features/precompute_win_ratios.R')

# This will precompute win ratios always with respect to last 3 matches 
# and save them in the data/ directory as "matches_with_win_ratio_from_prev_3_matches.csv"

# create file only if doesnt exist
if (!file.exists('./data/matches_with_win_ratio_from_prev_3_matches.csv')) {
  precompute.win.ratios.from.prev.matches(num.of.prev.matches.for.ratio = 3)
}
