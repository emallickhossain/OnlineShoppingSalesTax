# This gets the full Nielsen panel from 2004-2015
library(data.table)
library(stringr)
library(purrr)

nielsenPanel <- function(year) {
  panelists <- fread(paste0('/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/',
                            year, '/Annual_Files/panelists_', year, '.tsv'),
                     select = c('household_code', 'panel_year', 'projection_factor',
                                'household_income', 'race', 'fips_state_code',
                                'fips_state_descr', 'fips_county_code',
                                'household_internet_connection', 'female_head_age',
                                'male_head_age', 'panelist_zip_code'))
  panelists[, c('age', 'fips', 'female_head_age', 'male_head_age',
                'fips_state_code', 'fips_county_code') :=
              .(pmax(female_head_age, male_head_age),
                as.integer(paste0(fips_state_code,
                                  str_pad(fips_county_code, 3, pad = 0))),
                NULL, NULL, NULL, NULL)]
}

full_panel <- rbindlist(map(2004:2015, nielsenPanel))
setnames(full_panel, c('household_income', 'fips_state_descr', 'household_internet_connection'),
         c('income', 'state', 'internet'))

# Cleaning up income data because Nielsen added and then removed additional categories
full_panel[income >= 27, 'income' := 27L]
save(full_panel, file = '/home/mallick/Desktop/Nielsen/Data/fullPanel.rda',
     compress = TRUE)
rm(full_panel)
