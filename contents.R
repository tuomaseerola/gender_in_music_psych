# contents
# T. Eerola
# 01/08/2025
# Gender analysis of MusicPsych authors

#### LIBRARIES ----------------------------
library(tidyverse)
library(stringr)
library(ggplot2)
library(bib2df)
library(countrycode)
library(DescTools)
library(here)

#### DATA ----------------------------------
rm(list = ls())

#### GET A NEW DATASET WITH FULL FIRST NAMES --------
source('scripts/load_scopus_datasets.R') # Creates full_names dataset
source('scripts/attribute_country.R') # Add country affiliations
source('scripts/attribute_gender.R') # Adds gender attributions (precalcutaed with API)
source('scripts/name_gender_fixes.R') # Fixes the gender attribution of some names manually 
source('scripts/create_keys.R') # Add keys for authors and study+author
source('scripts/clean_citations_and_OA.R') # Process citations and OA status
source('scripts/attribute_gender_diagnostics.R') # Calculate how reliable is the attribution
source('scripts/export_gender_data.R') # save data (remove emails)

#### Analyse --------
source('scripts/load_gender_data.R') # Loads the data
source('scripts/summarise_gender.R') # removed modelsummary
source('scripts/quantify_authorship.R') # Added overall. Add psych data?
source('scripts/visualise_gender.R') # Which representation we prefer?
#source('scripts/visualise_continent.R') # Is this needed?

#source('scripts/visualise_names.R') # Bonus feature
source('scripts/keyword_analysis.R') # creates 3 alternative figures
