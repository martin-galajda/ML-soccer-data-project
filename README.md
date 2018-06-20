# Soccer data analysis and predictions

## About
This is github repository for project for "Machine learning" class at FIB UPC.

## Team
- Martin Galajda
- Florian König
- Aitor Ortiz de Latierro

# Guidelines working with the projects
- ⚠️ When working with R scripts, working directory is set to the project root (all code should follow this convention! 🙏)

# Directory structure

## models/
- The purpose of this directory is to contain models which will be built for different predictions (match result, goals scored, player analytics)

- ## models/match_result
  - predicting match result (win, tie, loss)
- ## models/goals_scored
  - predicting goals scored

## loaders/
- The purpose of this directory is to define functions which will load different data with different features (used in models). Possibly uses precomputed features which take a bit time to compute.

## feature_extraction/
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

## data/
- The purpose of this directory is to contain all data for predicting / analysing
- Specifically, we store here even precomputed features which are not easily computed on the run (take ~ 5 min)

## data_conversion/
- The purpose of this directory is to contain all code that extracts usable data from the SQLite database.

## validation/
- The purpose of this directory is to contain all code related to validation and selecting right models/features. Some of the scripts compute CV-errors for different subset of features/ parameters and save them as Rdata (because it takes a lot of time to compute).

## results/
- The purpose of this directory is to observe obtained results (for match_result). It uses saved RData to present obtained results from cross-validation and test error from best model.

# How to run

## Requirements
- R environment (RStudio preferably)

## Steps
- set root of the project as working directory (some scripts require that)
- run the code

## Interesting scripts to run
- `data_conversion\X.R` generates necessary `data\db_X.CSV` database from original `data/database.sqlite`. (downloadable at [Kaggle](https://www.kaggle.com/hugomathien/soccer))
- `validation/match_result/*.R` - scripts contain logic for feature selection, tuning parameters for different models. However, execution time is very big for some scripts (therefore they store results in `results/` directory after computing).
- `results/match_results/validation/*.R` - scripts contain interpretation of results achieved by running scripts from `validation/match_result.R` scripts. They use saved RData from `results/match_results/saved_Rdata` directory.
- `results/match_result/test_error_best_model.R` - script computing test error for best model for predicting match results
- `results/match_result/compare_models.R` - script comparing different models for predicting match result and observing their respective CV-errors.
- `analyze_players.R` - full code for Player Analysis section
- `models/goals_scored/*.R` - self-contained scripts to tune and train the different number of goals models including the baseline + `goals_distribution.R` plot script.
