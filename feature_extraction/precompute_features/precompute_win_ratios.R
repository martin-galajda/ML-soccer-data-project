source('./feature_extraction/win_ratios.R')
source('./loaders/load_matches_from_csv.R')

# how often log progress
log.period <- 100

precompute.win.ratios.from.prev.matches <- function(num.of.prev.matches.for.ratio = 3) {
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
    win.ratios <- compute_win_ratio_for_match(ordered.matches.by.date, match, num.of.prev.matches.for.ratio = 3)
    
    enhanced.match <- cbind(match, win.ratios)
    enhanced.matches <- rbind(enhanced.matches, enhanced.match)
    
    if (i %% log.period == 0) {
      print(paste("Processed ", i, " out of ", dim(matches.without.first.season)[1]))
    }
  }
  
  # get rid of some for which we do not have sufficient data (win ratio is NA)
  # should be getting rid of approx 210 matches (so not so much)
  matches.with.ratios <- enhanced.matches[!is.na(enhanced.matches$win.ratio.home.with.ties), ]
  matches.with.ratios <- matches.with.ratios[!is.na(matches.with.ratios$win.ratio.away.with.ties), ]
  
  # lets write it
  filename <- paste("data/matches_with_win_ratio_from_prev_", num.of.prev.matches.for.ratio , "_matches.csv", sep = "")
  write.csv(matches.with.ratios, filename)
}

