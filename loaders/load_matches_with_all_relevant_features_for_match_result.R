#' Loads matches with all features relevant for predicting match result (home win, tie, away in)
#' @param number.of.prev.matches.for.win.ratio[=3] value determining how many prev matches use for computing win ratio (3/10)
#' @return matches with all relevant features
load.matches.with.all.features.for.match.result <- function(number.of.prev.matches.for.win.ratio = 3) {
  source('./loaders/load_matches_with_attack_ratios.R')
  source('./loaders/load_matches_with_win_ratios_prev_3.R')
  source('./loaders/load_matches_with_win_ratios_prev_3_tie_as_half_win.R')
  source('./loaders/load_matches_with_win_ratios_prev_10_tie_as_half_win.R')
  
  
  matches.with.attack.ratios <- load.matches.with.attack.ratios()
  
  if (number.of.prev.matches.for.win.ratio == 3) {
    matches.with.win.ratios.with.ties <- load.matches.with.win.ratio.from.prev.3.matches.with.tie.as.half()
  } else if (number.of.prev.matches.for.win.ratio == 10) {
    matches.with.win.ratios <- load.matches.with.win.ratio.from.prev.10.matches.with.tie.as.half()
  }

  shared.features <- c("X", "id", "result", "season", "home_team", "away_team", "league", "date")
  
  # merge dataframes containing attack ratios and win ratios
  matches.merged.all.features <- matches.with.attack.ratios
  matches.merged.all.features <- merge(matches.merged.all.features, matches.with.win.ratios.with.ties, by = shared.features, all = TRUE)
  
  # get rid of possible NAs created during merging
  matches.merged.all.features <- matches.merged.all.features[complete.cases(matches.merged.all.features), ]

  return(matches.merged.all.features)
}