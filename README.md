Analysis of braided river bird species on the Rakitata
-
Contains code for running analysis in R:
  - read_organise_data.R loads all relevant data and manipulates it into a format for analysis
  - /species_models contains a script for each species that runs a GLMM and model selection process, eventually giving a test model for power analysis
  - power_analysis.R sources all species models and runs a power analysis on each
  - try_historic_data.R adjusts the older patchy data and runs GLMM on one species- will expand to others soon
/power_analyses:
  - contains csv files of the power analysis results for each species
/data:
  - contains count and flow data
/plots:
  - contains visualisations of models and power analyses
/other-docs:
  - contains other documents that are relevant to the project regarding methods and context
