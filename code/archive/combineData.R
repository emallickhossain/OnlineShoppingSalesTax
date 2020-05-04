# ------------------ ABOUT -----------------------------------------------------
# This file takes the comScore Transaction and Demographics data and combines it
# with the Thomson Reuters tax data, the Census county adjacency file, and
# the Missouri Census Data Center Geographic Correspondence Engine to obtain
# the final data set on which I conduct my analysis.

# ----------------- CODE -------------------------------------------------------
library(data.table)
library(readxl)
library(haven)

# Load transaction and demographic data
load("/home/mallick/Desktop/comScore/DemographicsClean.rda")
load("/home/mallick/Desktop/comScore/TransactionsClean.rda")

# Only keeping machine_id, income, age, race, ZIP code, and year from demographics
demographics <- demographics[, c("machine_id", "household_income_coarse",
                                 "hoh_oldest_age_coarse", "racial_background",
                                 "zip_code", "year"), with = TRUE]

# Only keeping machine_id, domain, product category, product price,
# basket total, date, year, and month from transactions
transactions <- transactions[, c("domain_name", "domain_id", "machine_id",
                                 "prod_category_id", "prod_totprice_real",
                                 "basket_tot_real", "event_date",
                                 "year", "month", "Offline"), with = TRUE]

# Merging transactions and demographics together
full_data <- merge(transactions, demographics, by = c("machine_id", "year"))
rm(transactions, demographics)

# Merging with browsing activity
load("/home/mallick/Desktop/comScore/fullTransactionBrowsing.rda")
full_data <- merge(full_data, full_transaction_browsing,
                   by = c("year", "month", "machine_id", "domain_name", "domain_id"))
rm(full_transaction_browsing)

# Loading ZIP and county correspondence file and removing invalid 99999 codes
zip_fip <- fread("/home/mallick/Dropbox/Research/OnlineShopping/code/0_data/zip_fips_2010_missouri_center.csv")
zip_fip <- zip_fip[-1]
zip_fip[, c("state", "cntyname", "zipname", "pop10", "afact") := NULL]
setnames(zip_fip, c("zip_code", "fips", "state"))
zip_fip[, c("zip_code", "fips") := .(as.numeric(zip_code), as.numeric(fips))]
zip_fip <- zip_fip[zip_code != 99999]
zip_fip <- unique(zip_fip, by = "zip_code")
zip_fip[, c("zip_code", "fips") := .(as.integer(zip_code), as.integer(fips))]

# Merging county codes with ZIP codes in data
full_data <- merge(full_data, zip_fip, by = "zip_code")
rm(zip_fip)

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("/home/mallick/Dropbox/Research/OnlineShopping/code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, c("state", "LegallyRequired", "AmazonCollected",
                             "OverstockCollected"), with = TRUE]
amazon_tax[, c("LegallyRequired", "AmazonCollected", "OverstockCollected") :=
             .(as.Date(LegallyRequired), as.Date(AmazonCollected),
               as.Date(OverstockCollected))]
full_data <- merge(full_data, amazon_tax, by = "state")
rm(amazon_tax)

# Merging with tax rates (no tax info is available outside of 2006-2014)
load("/home/mallick/Desktop/comScore/zip_tax_min.rda")
full_data <- merge(full_data, final_zip_tax, by = c("year", "month", "fips"), all.x = TRUE)
full_data[, "fips" := as.numeric(fips)]
rm(final_zip_tax)

# Generating an indicator for Amazon (Zappos became a subsidiary of Amazon on July 22, 2009)
full_data[, "amazon" := ifelse(domain_name == "amazon.com", 1L, 0L)]
full_data[domain_name == "zappos.com" & event_date >= as.Date("2009-07-22"), "amazon" := 1L]

# Generating an indicator for Overstock
full_data[, "overstock" := ifelse(domain_name == "overstock.com", 1L, 0L)]

# Generating an indicator for whether Amazon collects sales tax in the state
full_data[, "amazon_collect" := ifelse(event_date >= AmazonCollected, 1L, 0L)]
full_data[is.na(amazon_collect), "amazon_collect" := 0L]

# Generating an indicator for whether Overstock collects sales tax in the state
full_data[, "overstock_collect" := ifelse(event_date >= OverstockCollected, 1L, 0L)]
full_data[is.na(overstock_collect), "overstock_collect" := 0L]

# Converting "Offline" indicator to a measure of whether tax is collected
full_data[, "taxed_nonamazon" := ifelse(Offline == "Yes", 1L, 0L)]
full_data[overstock == 1, "taxed_nonamazon" := overstock * overstock_collect]
full_data[, "nontaxed_nonamazon" := ifelse(taxed_nonamazon == 0 & amazon == 0, 1L, 0L)]

# Adding month counter to get month fixed effects for later
full_data[, "month_counter" := (year - 2006L) * 12L + month]

# Generating border county indicator
full_data[, "border_county" := 0L]
full_data[min_adj_tax == 0 & sales_tax > 0, "border_county" := 1L]

# Generating tax diff and tax ratio
full_data[, c("tax_diff") := .(sales_tax - min_adj_tax)]

# Renaming age and race
setnames(full_data, c("racial_background", "hoh_oldest_age_coarse"), c("race", "age"))

# Saving
save(full_data, file = "/home/mallick/Desktop/comScore/regressionData.rda", compress = TRUE)
rm(full_data)
