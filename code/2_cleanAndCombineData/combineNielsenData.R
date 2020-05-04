# ------------------ ABOUT -----------------------------------------------------
# This file takes the Nielsen Trip and Panelist data and combines it
# with the Thomson Reuters tax data, the Census county adjacency file, and
# the Missouri Census Data Center Geographic Correspondence Engine to obtain
# the final data set on which I conduct my analysis.

# ----------------- CODE -------------------------------------------------------
library(data.table)
library(readxl)
library(haven)
library(lubridate)
library(fredr)
library(maps)
library(stringr)
# fredr_set_key(fredAPI)

householdSpending <- fread("/scratch/upenn/hossaine/comScore/NielsenMonthlySpend.csv")
demographics <- fread("/scratch/upenn/hossaine/comScore/NielsenFullPanel.csv")
demographics[, "fips" := str_pad(fips, 5, "left", "0")]
demographics[, "state_fips" := as.integer(substr(fips, 1, 2))]
st_fips <- unique(as.data.table(state.fips)[, .(state_fips = fips, state = abb)])
extra_states <- data.table(state_fips = c(2, 15, 72), state = c("AK", "HI", "PR"))
st_fips <- rbindlist(list(st_fips, extra_states), use.names = TRUE)
demographics <- merge(demographics, st_fips, by = "state_fips")

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("/scratch/upenn/hossaine/comScore/AmazonLaws.xls"), key = "state")
amazon_tax <- amazon_tax[, .(state, AmazonCollected)]
amazon_tax[, c("AmazonCollected") := .(as.Date(AmazonCollected, format = "%Y-%m-%d"))]
demographics <- merge(demographics, amazon_tax, by = "state")
householdSpending <- merge(demographics, householdSpending,
                           by = c("household_code", "panel_year"))
rm(amazon_tax)

# Merging with tax rates (no tax info is available outside of 2006-2016)
final_zip_tax <- fread("/scratch/upenn/hossaine/comScore/zip_tax_min.csv")
householdSpending[, "fips" := as.integer(fips)]
householdSpending <- merge(householdSpending, final_zip_tax,
                           by = c("year", "month", "fips"))
rm(final_zip_tax)

# Adding in tax differences
householdSpending[, c("tax_diff") := .(sales_tax - min_adj_tax)]

# Generating an indicator for whether Amazon collects sales tax in the state
householdSpending[, "date" := as.Date(paste(year, month, "01", sep = "-"))]
householdSpending[, "amazon_collect" := ifelse(date >= AmazonCollected, 1L, 0L)]
householdSpending[is.na(amazon_collect), "amazon_collect" := 0L]

# Deflating prices to December 2016
cpi <- fread("/home/upenn/hossaine/Nielsen/Data/cpi.csv")
cpi[, "date" := as.Date(date)]
setnames(cpi, "value", "cpi")
householdSpending <- merge(householdSpending, cpi, by = "date")
dec2016 <- cpi[date == "2016-12-01"]$cpi
householdSpending[, ':=' (total_real = total / cpi * dec2016,
                          onlineGroc_real = onlineGroc / cpi * dec2016,
                          offlineGroc_real = offlineGroc / cpi * dec2016,
                          onlineNonGroc_real = onlineNonGroc / cpi * dec2016,
                          offlineNonGroc_real = offlineNonGroc / cpi * dec2016)]

# Adding month counter to get month fixed effects for later
householdSpending[, "month_counter" := as.integer((panel_year - 2006) * 12 + month)]
fwrite(householdSpending, file = "/scratch/upenn/hossaine/comScore/regressionDataAll.csv")
