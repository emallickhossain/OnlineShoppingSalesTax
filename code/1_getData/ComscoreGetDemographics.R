# ---------------- ABOUT -------------------------------------------------------
# This gets all demographic data from comScore by year and drops any households
# with incomplete demographic information or invalid ZIP codes

# ---------------- CODE --------------------------------------------------------
# Import libraries
library(data.table)
library(maps)
library(purrr)
library(stringr)
library(stargazer)

hhs <- fread("./code/0_data/Clean/comScoreTransactionReg.csv")
amazon_purch_hhs <- uniqueN(hhs$machine_id)

# Vector of years to loop over
years <- c(2006:2016)

# Pulling and appending all demographic data, after adding a year column
getDems <- function(year, sampleSize = -1) {
  res <- dbSendQuery(wrds, paste0("SELECT * FROM comscore.demographics", year))
  data <- setDT(dbFetch(res, n = sampleSize))[, "year" := year]
  dbClearResult(res)
  data <- data[, lapply(.SD, as.integer)]
  return(data)
}
demographics <- rbindlist(purrr::map(years, getDems), use.names = TRUE)

# ------------------------------- CLEANING -------------------------------------
# Removing education because it's not well populated
# Removing connection speed and Census region because these are not used
demographics[, c("hoh_most_education", "census_region", "connection_speed") := NULL]
row1 <- c("Starting HH:", uniqueN(demographics$machine_id))

# Removing unknown demographics
demographics <- demographics[household_size %in% c(1:6) &
                               hoh_oldest_age %in% c(1:11) &
                               racial_background %in% c(1, 2, 3, 5)]
row2 <- c("Complete demographics:", uniqueN(demographics$machine_id))

# Removing invalid ZIP codes
demographics <- demographics[!is.na(zip_code) & zip_code != 99999]

# Renaming variables
setnames(demographics, c("household_size", "hoh_oldest_age", "household_income",
                         "racial_background", "country_of_origin"),
         c("size", "age", "income", "race", "hispanic"))

# Adding in ZIP code and collapsing to ZIP
zipFip <- fread(paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/",
                       "zcta_county_rel_10.txt"),
                select = c("ZCTA5", "STATE", "COUNTY", "COPOPPCT"))
zipFip[, "fips" := paste0(str_pad(STATE, 2, "left", "0"),
                          str_pad(COUNTY, 3, "left", "0"))]
zipFip <- zipFip[, .SD[which.max(COPOPPCT)], by = ZCTA5]
zipFip[, c("STATE", "COUNTY", "COPOPPCT") := NULL]
setnames(zipFip, "ZCTA5", "zip_code")
demographics <- merge(demographics, zipFip, by = "zip_code")

# Final count after merging
row3 <- c("Valid ZIPs:", uniqueN(demographics$machine_id))

# Adding Amazon purchase filter
row4 <- c("Made Online Purchase:", amazon_purch_hhs)

cleanTable <- as.data.table(rbind(row1, row2, row3, row4))
setnames(cleanTable, c("Step", "HH"))
cleanTable[, "HH" := as.integer(HH)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "comScore Sample",
          label = "tab:comScoreClean", digits = 2, rownames = FALSE,
          out = "./tables/comScoreClean.tex")

# Adding state fips code
demographics[, "state_fips" := as.integer(substr(fips, 1, 2))]
st_fips <- unique(as.data.table(state.fips)[, .(state_fips = fips, state = abb)])
extra_states <- data.table(state_fips = c(2, 15, 72), state = c("AK", "HI", "PR"))
st_fips <- rbindlist(list(st_fips, extra_states), use.names = TRUE)
demographics <- merge(demographics, st_fips, by = "state_fips")[, "state_fips" := NULL]

# Save
fwrite(demographics, file = "./code/0_data/Clean/DemographicsClean.csv")
