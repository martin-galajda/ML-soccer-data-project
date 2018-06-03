compute_tie_ratio <- function(matches.team.played.home, matches.team.played.away) {
  matches.tied.playing.home <- matches.team.played.home[matches.team.played.home$result == "Tie", ]
  matches.tied.playing.away <- matches.team.played.away[matches.team.played.away$result == "Tie", ]

  tie.ratio.playing.home <- dim(matches.tied.playing.home)[1] / dim(matches.team.played.home)[1]
  tie.ratio.playing.away <- dim(matches.tied.playing.away)[1] / dim(matches.team.played.away)[1]
  tie.ratio <- 
    (dim(matches.tied.playing.home)[1] + dim(matches.tied.playing.away)[1]) /
    (dim(matches.team.played.home)[1] + dim(matches.team.played.away)[1])


  result <- data.frame(tie.ratio, tie.ratio.playing.home, tie.ratio.playing.away)

  return(result)
}

compute_win_ratio <- function(matches.team.played.home, matches.team.played.away, with.tie.as.half.win) {
  matches.won.team.playing.home <- matches.team.played.home[
    matches.team.played.home$result == "Home win",
  ]
  matches.won.team.playing.away <- matches.team.played.away[
    matches.team.played.away$result == "Away win",
  ]

  number.of.matches.won.home <- dim(matches.won.team.playing.home)[1]
  number.of.matches.won.away <- dim(matches.won.team.playing.away)[1]
  number.of.matches.won <- number.of.matches.won.home + number.of.matches.won.away

  number.of.all.matches <- dim(matches.team.played.home)[1] + dim(matches.team.played.away)[1]

  if (with.tie.as.half.win) {
    matches.tied.team.playing.home <- matches.team.played.home[
      matches.team.played.home$result == "Tie",
    ]

    matches.tied.team.playing.away <- matches.team.played.away[
      matches.team.played.away$result == "Tie",
    ]

    number.of.matches.won.home <- number.of.matches.won.home + (dim(matches.tied.team.playing.home)[1] * 0.5)
    number.of.matches.won.away <- number.of.matches.won.away + (dim(matches.tied.team.playing.away)[1] * 0.5)
    number.of.matches.won <- number.of.matches.won + (dim(matches.tied.team.playing.home)[1] * 0.5) + (dim(matches.tied.team.playing.away)[1] * 0.5)
  }

  win.ratio <- number.of.matches.won / number.of.all.matches
  win.ratio.playing.home <- number.of.matches.won.home / dim(matches.team.played.home)[1]
  win.ratio.playing.away <- number.of.matches.won.away / dim(matches.team.played.away)[1]

  result <- data.frame(win.ratio, win.ratio.playing.home, win.ratio.playing.away)

  return(result)
}

# Compute ratios of matches won / tied for given match.
# Returns results as data frame.
compute_win_ratio_for_match <- function(matches, match, num.of.prev.matches.for.ratio = 3, with.tie.as.half.win = FALSE) {
  # we expect matches to be ordered by date
  ordered.matches <- matches[order(matches$date), ]
  
  filtered.matches.home.team.playing.home <- ordered.matches[
    (as.character(ordered.matches$home_team) == match$home_team)
    & as.Date(ordered.matches$date) < match$date,
  ]

  filtered.matches.home.team.playing.away <- ordered.matches[
    (as.character(ordered.matches$away_team) == match$home_team)
    & as.Date(ordered.matches$date) < match$date,
  ]
  
  filtered.matches.away.team.playing.home <- ordered.matches[
    (as.character(ordered.matches$home_team) == match$away_team )
    & as.Date(ordered.matches$date) < match$date,
  ]

  filtered.matches.away.team.playing.away <- ordered.matches[
    (as.character(ordered.matches$away_team) == match$away_team)
    & as.Date(ordered.matches$date) < match$date,
  ]
  
  # We want to take into account only last "num.of.prev.matches.for.ratio" matches
  filtered.matches.home.team.playing.home <- tail(filtered.matches.home.team.playing.home, num.of.prev.matches.for.ratio)
  filtered.matches.home.team.playing.away <- tail(filtered.matches.home.team.playing.away, num.of.prev.matches.for.ratio)
  filtered.matches.away.team.playing.home <- tail(filtered.matches.away.team.playing.home, num.of.prev.matches.for.ratio)
  filtered.matches.away.team.playing.away <- tail(filtered.matches.away.team.playing.away, num.of.prev.matches.for.ratio)  

  # compute win ratios
  win.ratios.home.team <- compute_win_ratio(filtered.matches.home.team.playing.home, filtered.matches.home.team.playing.away, with.tie.as.half.win)
  win.ratios.away.team <- compute_win_ratio(filtered.matches.away.team.playing.home, filtered.matches.away.team.playing.away, with.tie.as.half.win)
  # create home team ratios
  win.ratio.home.team <- win.ratios.home.team$win.ratio
  win.ratio.home.team.playing.home <- win.ratios.home.team$win.ratio.playing.home
  win.ratio.home.team.playing.away <- win.ratios.home.team$win.ratio.playing.away
  # create away team ratios
  win.ratio.away.team <- win.ratios.away.team$win.ratio
  win.ratio.away.team.playing.home <- win.ratios.away.team$win.ratio.playing.home
  win.ratio.away.team.playing.away <- win.ratios.away.team$win.ratio.playing.away

  # ties are included in win ratio, do not compute them
  if (with.tie.as.half.win == TRUE) {
    result = data.frame(
      win.ratio.home.team,
      win.ratio.home.team.playing.home,
      win.ratio.home.team.playing.away,

      win.ratio.away.team,
      win.ratio.away.team.playing.home,
      win.ratio.away.team.playing.away
    )
  
    return(result)
  }

  # compute tie ratios
  tie.ratios.home.team <- compute_tie_ratio(filtered.matches.home.team.playing.home, filtered.matches.home.team.playing.away)
  tie.ratios.away.team <- compute_tie_ratio(filtered.matches.away.team.playing.home, filtered.matches.away.team.playing.away)
  # create home team ratios
  tie.ratio.home.team <- tie.ratios.home.team$tie.ratio
  tie.ratio.home.team.playing.home <- tie.ratios.home.team$tie.ratio.playing.home
  tie.ratio.home.team.playing.away <- tie.ratios.home.team$tie.ratio.playing.away
  # create away team ratios
  tie.ratio.away.team <- tie.ratios.away.team$tie.ratio
  tie.ratio.away.team.playing.home <- tie.ratios.away.team$tie.ratio.playing.home
  tie.ratio.away.team.playing.away <- tie.ratios.away.team$tie.ratio.playing.away
  
  result = data.frame(
    win.ratio.home.team,
    win.ratio.home.team.playing.home,
    win.ratio.home.team.playing.away,

    win.ratio.away.team,
    win.ratio.away.team.playing.home,
    win.ratio.away.team.playing.away,

    tie.ratio.home.team,
    tie.ratio.home.team.playing.home,
    tie.ratio.home.team.playing.away,

    tie.ratio.away.team,
    tie.ratio.away.team.playing.home,
    tie.ratio.away.team.playing.away
  )
  
  return(result)
}

# Example usage:
# ratios = compute_ratios(matches_csv, start.date = as.Date("1990-10-15"), end.date = as.Date("2018-01-14"), team = "PEC Zwolle", against.team = "FC Groningen")
