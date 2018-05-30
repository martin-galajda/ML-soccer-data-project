# Soccer data analysis and predictions

## About
This is github repository for project for "Machine learning" class at FIB UPC.

## Team
- Martin Galajda
- Florian König
- Aitor Ortiz de Latierro

# Guidelines working with the projects
- ⚠️ Make sure that when working with R scripts, working directory is set to the root of the project (all R scripts should follow this convention! 🙏)

# Directory structure

## models/
- The purpose of this directory is to contain models which will be built for different predictions (match result, goals scored, player analytics)

- ## models/match_result
  - predicting match result (win, tie, loss)
- ## models/goals_scored
  - predicting goals scored
- ## models/match_win (yes / no)
  - predict if home team will win or no
## loaders/
- The purpose of this directory is to define functions which will load different data with different features (used in models). Possibly uses precomputed features which take a bit time to compute.

## feature_extraction
- The purpose of this directory is to define everything related to feature extraction.
- ## precompute_features/
  - this directory contains functions which when executed (source(path) in R) will make sure that cached precomputed features are present in data/ directory
  - this are used by some loaders to make sure that precomputed features are saved in csv file in data/ directory
  - ## precompute_attack_strength_ratios.R
    - compute attack strength ratio for each match by taking into account all matches from past (so we don't leak future information into our data)
    - saves precomputed features in data/ directory
  - ## precompute_win_ratio.R
    - compute win ratios for given match
    - takes parameter which determines how many matches from past to take into account
    - saves precomputed features in data/ directory


## data
- The purpose of this directory is to contain all data for predicting / analysing
- Specifically, we store here even precomputed features which are not easily computed on the run (take ~ 5 min)