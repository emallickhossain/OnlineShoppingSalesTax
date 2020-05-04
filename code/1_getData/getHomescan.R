# All transferring, unzipping and cleaning of Homescan data
# find /scratch/upenn/hossaine/nielsen_extracts/ -exec touch {} \;
library(data.table)
library(purrr)
library(stringr)
library(readxl)
library(stargazer)
library(geosphere)
yrs <- 2006:2016
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
threads <- 8

######################## STEP 1: TRANSFER ######################################
# Transfer homescan data from Globus to WRDS scratch
################################################################################

######################## STEP 2: UNZIP #########################################
# cd /scratch/upenn/hossaine
# tar -xvzf Consumer_Panel_Data_2004_2017.tgz
################################################################################

######################## STEP 3: CLEAN PANELIST FILE ###########################
# Final sample removes any students and military as well as various missing
# entries based on other data combined with the panel data
getPanel <- function(yr) {
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"),
                 nThread = threads,
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor",
                            "Household_Income", "Household_Size",
                            "Age_And_Presence_Of_Children",
                            "Female_Head_Education", "Male_Head_Education",
                            "Male_Head_Occupation", "Female_Head_Occupation",
                            "Male_Head_Birth", "Female_Head_Birth",
                            "Marital_Status", "Race", "Hispanic_Origin",
                            "Panelist_ZipCd", "Fips_State_Cd", "Fips_County_Cd"))
  setnames(panel, tolower(names(panel)))
  panel[, "fips" := paste0(str_pad(fips_state_cd, 2, "left", "0"),
                           str_pad(fips_county_cd, 3, "left", "0"))]
  panel[, c("fips_state_cd", "fips_county_cd") := NULL]
  setnames(panel, c("household_cd", "panelist_zipcd"), c("household_code", "zip_code"))
  return(panel)
}

# Initial sample
panel <- rbindlist(map(yrs, getPanel))
row1 <- c("Starting HH:", uniqueN(panel$household_code))

# Removing military and students
panel <- panel[!male_head_occupation %in% c(7, 10)]
panel <- panel[!female_head_occupation %in% c(7, 10)]
row2 <- c("Exclude military and students:", uniqueN(panel$household_code))

# Removing households making under $5k
panel <- panel[household_income != 3]
row3 <- c("Exclude Households under 5k:", uniqueN(panel$household_code))

# Adding income factors
panel[, "household_income_coarse" := cut(household_income, c(0, 13, 19, 26, 30),
                                         labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                         ordered_result = TRUE)]
panel[, "household_income" := ifelse(household_income >= 27, 27, household_income)]

# Adding age
panel[, "age" := panel_year - (female_head_birth + male_head_birth) / 2]
panel[is.na(age), "age" := as.numeric(panel_year - female_head_birth)]
panel[is.na(age), "age" := as.numeric(panel_year - male_head_birth)]

# Adding college indicator if at least 1 HoH has graduated college
panel[, "college" := 0]
panel[female_head_education >= 5 | male_head_education >= 5, "college" := 1]

# Making race binary
panel[, "white" := ifelse(race == 1, 1L, 0L)]

# Adding child indicator
panel[, "child" := ifelse(age_and_presence_of_children == 9, 0L, 1L)]

# Adding marriage indicator
panel[, "married" := ifelse(marital_status == 1, 1L, 0L)]

# Housekeeping
panel <- panel[, .(household_code, panel_year, projection_factor, household_income,
                   household_size, child, hispanic_origin, fips,
                   household_income_coarse, age, college, white, married)]
fwrite(panel, "/scratch/upenn/hossaine/comScore/NielsenFullPanel.csv", nThread = threads)

cleanTable <- as.data.table(rbind(row1, row2, row3))
setnames(cleanTable, c("Step", "HH"))
cleanTable[, "HH" := as.integer(HH)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Homescan Sample",
          label = "tab:homeScanClean", digits = 2, rownames = FALSE,
          out = "/home/upenn/hossaine/NewComScore/tables/homeScanClean.tex")

######################## STEP 4: CLEAN PRODUCTS FILE ###########################
# Final sample removes any deferred modules, magnet categories, alcohol and tobacco
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))
row0 <- c("Starting Categories", uniqueN(prod$product_module_code))

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]
row1 <- c("Excluding Deferred Categories", uniqueN(prod$product_module_code))

# Excluding all alcohol, tobacco, and magnet purchases
prod <- prod[department_code != 8] #alcohol
row2 <- c("Excluding Alcohol", uniqueN(prod$product_module_code))

prod <- prod[product_group_code != 4510] #tobacco
row3 <- c("Excluding Tobacco", uniqueN(prod$product_module_code))

prod <- prod[!product_module_code %in% c(445:468, 750)]
row4 <- c("Excluding Magnet Purchases", uniqueN(prod$product_module_code))

# Generating food or non-food flag
prod[, "nonGroc" := ifelse(department_code %in% c(0, 7, 9), 1L, 0L)]
prod <- prod[, .(upc, upc_ver_uc, department_code, nonGroc)]

fwrite(prod, "/scratch/upenn/hossaine/comScore/fullProd.csv", nThread = threads)

cleanTable <- as.data.table(rbind(row0, row1, row2, row3, row4))
setnames(cleanTable, c("Step", "Categories"))
cleanTable[, "Categories" := as.integer(Categories)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Nielsen Product Categories",
          label = "tab:prodClean", digits = 2, rownames = FALSE,
          out = "/home/upenn/hossaine/NewComScore/tables/prodClean.tex")

######################## STEP 5: CLEAN PURCHASES FILE ##########################
# Keeping purchases with positive prices and computing total spending by
# grocery/non-grocery and trip
getPurch <- function(yr) {
  print(yr)
  # Getting purchases and combining the same products purchased on the same trip
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 nThread = threads, key = c("trip_code_uc", "upc", "upc_ver_uc"),
                 select = c("trip_code_uc", "upc", "upc_ver_uc", "total_price_paid"))

  purch <- purch[, .(total_price_paid = sum(total_price_paid)),
                 by = .(trip_code_uc, upc, upc_ver_uc)]

  # Keeping those with positive prices
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))[total_price_paid > 0]

  # Aggregating by groc/nonGroc and trip
  purch <- purch[, .(total_spend = sum(total_price_paid)),
                 by = .(trip_code_uc, nonGroc)]

  return(purch)
}

fullPurch <- rbindlist(purrr::map(yrs, getPurch), use.names = TRUE)

# Saving
fwrite(fullPurch, "/scratch/upenn/hossaine/comScore/fullPurch.csv", nThread = threads)

######################## STEP 6: RETAILERS FILE ################################
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fwrite(retailers, "/scratch/upenn/hossaine/comScore/fullRetailers.csv", nThread = threads)

######################## STEP 7: CLEAN TRIPS FILE ##############################
retailers <- fread("/scratch/upenn/hossaine/comScore/fullRetailers.csv", nThread = threads)

fullTrips <- NULL
for (yr in yrs) {
  print(yr)
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"), nThread = threads)
  trips <- trips[, .(household_code, panel_year, retailer_code, trip_code_uc, purchase_date)]
  trips <- merge(trips, retailers, by = "retailer_code")
  fullTrips <- rbindlist(list(fullTrips, trips), use.names = TRUE)
}

fwrite(fullTrips, "/scratch/upenn/hossaine/comScore/fullTrips.csv", nThread = threads)

####################### STEP 8: COMBINING ALL NIELSEN ##########################
# Aggregating online and offline spending to the monthly level

# Loading data
panel <- fread("/scratch/upenn/hossaine/comScore/NielsenFullPanel.csv", nThread = threads)
purch <- fread("/scratch/upenn/hossaine/comScore/fullPurch.csv", nThread = threads)
trips <- fread("/scratch/upenn/hossaine/comScore/fullTrips.csv", nThread = threads)

# Aggregating trip spending to the year-month-channel level
fullData <- merge(purch, trips, by = "trip_code_uc")[, "trip_code_uc" := NULL]
fullData[, "onlineSpend" := ifelse(channel_type == "Online Shopping", total_spend, 0)]
fullData[, "offlineSpend" := total_spend - onlineSpend]
fullData[, c("year", "month") := .(as.integer(substr(purchase_date, 1, 4)),
                                   as.integer(substr(purchase_date, 6, 7)))]
monthlySpend <- fullData[, .(onlineSpend = sum(onlineSpend),
                             offlineSpend = sum(offlineSpend)),
                         by = .(household_code, panel_year, nonGroc, year, month)]

# Aggregating to log- total, online, offline, grocery, non-grocery spending
monthlySpend[, "onlineGroc" := (1 - nonGroc) * onlineSpend]
monthlySpend[, "offlineGroc" := (1 - nonGroc) * offlineSpend]
monthlySpend[, "onlineNonGroc" := nonGroc * onlineSpend]
monthlySpend[, "offlineNonGroc" := nonGroc * offlineSpend]
monthlySpend[, "totalSpend" := onlineGroc + offlineGroc + onlineNonGroc + offlineNonGroc]

spendingByType <- monthlySpend[, .(total = sum(totalSpend),
                                   onlineGroc = sum(onlineGroc),
                                   offlineGroc = sum(offlineGroc),
                                   onlineNonGroc = sum(onlineNonGroc),
                                   offlineNonGroc = sum(offlineNonGroc)),
                               by = .(household_code, panel_year, year, month)]

fwrite(spendingByType, "/scratch/upenn/hossaine/comScore/NielsenMonthlySpend.csv", nThread = threads)
