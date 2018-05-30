load.matches.with.all.features.for.match.result <- function() {
  source('./loaders/load_matches_with_attack_ratios.R')
  source('./loaders/load_matches_with_win_ratios_prev_3.R')
  
  matches.with.attack.ratios <- load.matches.with.attack.ratios()
  matches.with.win.ratios <- load.matches.win.with.ratio.from.prev.3.matches()

  shared.features <- c("id", "result", "season", "home_team", "away_team", "league", "date")
  
  # merge dataframes containing attack ratios and win ratios
  matches.merged.all.features <- matches.with.attack.ratios
  matches.merged.all.features <- merge(matches.merged.all.features, matches.with.win.ratios, by = shared.features, all = TRUE)
  
  # get rid of possible NAs created during merging
  matches.merged.all.features <- matches.merged.all.features[
    !is.na(matches.merged.all.features$win.ratio) &
      !is.na(matches.merged.all.features$attack.strength.home.team) &
      !is.na(matches.merged.all.features$attack.strength.away.team) , 
    ]
}