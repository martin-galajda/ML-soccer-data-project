source('./feature_extraction/win_ratios.R')
source('./loaders/load_matches_from_csv.R')

# how often log progress
log.period <- 100

#' Precompute win ratios for all matches using past matches and write it to csv file inside /data directory.
#' @param num.of.prev.matches.for.ratio[=3] how many matches from the past use for computing win ratio
#' @param with.tie.as.half.win[=FALSE] determines if tie should be counted as 0.5 win
precompute.win.ratios.from.prev.matches <- function(num.of.prev.matches.for.ratio = 3, with.tie.as.half.win = FALSE) {
  matches <- load.matches.from.csv()
  ordered.matches.by.date <- matches[order(matches$date), ]
  
  # keep just relevant feature
  relevant.features <- c("id", "result", "season", "home_team", "away_team", "league", "date")
  ordered.matches.by.date <- ordered.matches.by.date[, relevant.features]
  
  # lets get rid of matches from first season, as they miss win ratio for previous matches...
  first.season <- '2008/2009'
  matches.without.first.season <- ordered.matches.by.date[ordered.matches.by.date$season != first.season,]
  
  enhanced.matches <- data.frame()
  for (i in 1:(dim(matches.without.first.season)[1])) {
    match <- matches.without.first.season[i,]
    win.ratios <- compute_win_ratio_for_match(ordered.matches.by.date, match, num.of.prev.matches.for.ratio = 3, with.tie.as.half.win = with.tie.as.half.win)
    
    enhanced.match <- cbind(match, win.ratios)
    enhanced.matches <- rbind(enhanced.matches, enhanced.match)
    
    if (i %% log.period == 0) {
      print(paste("Processed ", i, " out of ", dim(matches.without.first.season)[1]))
    }
  }

  print(colnames(enhanced.matches))

  NAs.length <- dim(enhanced.matches[!complete.cases(enhanced.matches), ])[1]
  print(paste("Getting rid of ", NAs.length, " matches", sep = ""))
  
  # get rid of some for which we do not have sufficient data (win ratio is NA)
  matches.with.ratios <- enhanced.matches[complete.cases(enhanced.matches), ]

  # lets write it
  filename <- paste("data/matches_with_win_ratio_from_prev_", num.of.prev.matches.for.ratio , "_matches", sep = "")
  if (with.tie.as.half.win == TRUE) {
    filename <- paste(filename, "_with_tie_as_half_win", sep = "")
  }
  filename <- paste(filename, ".csv", sep = "")
  
  print(levels(matches.with.ratios$away_team))
  
  write.csv(matches.with.ratios, filename)
}

