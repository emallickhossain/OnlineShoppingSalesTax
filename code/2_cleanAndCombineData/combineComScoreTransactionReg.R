# ------------------ ABOUT -----------------------------------------------------
# This file takes the comScore Transaction and Demographics data and combines it
# with the Thomson Reuters tax data, the Census county adjacency file, and
# the Missouri Census Data Center Geographic Correspondence Engine to obtain
# the final data set on which I conduct my analysis.

# ----------------- CODE -------------------------------------------------------
library(data.table)
library(readxl)
library(stringr)

# Load transaction and demographic data
demographics <- fread("./code/0_data/Clean/DemographicsClean.csv")
transactions <- fread("./code/0_data/Clean/TransactionsClean.csv")
full_data <- merge(transactions, demographics, by = c("machine_id", "year"))
rm(transactions, demographics)
uniqueN(full_data$machine_id)

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, .(state, LegallyRequired, AmazonCollected, OverstockCollected)]
amazon_tax[, c("LegallyRequired", "AmazonCollected", "OverstockCollected") :=
             .(as.Date(LegallyRequired), as.Date(AmazonCollected),
               as.Date(OverstockCollected))]
full_data <- merge(full_data, amazon_tax, by = "state")
full_data[, "fips" := as.integer(fips)]
rm(amazon_tax)

# Merging with tax rates (no tax info is available outside of 2006-2016)
final_zip_tax <- fread("./code/0_data/Clean/zip_tax_min.csv")
full_data <- merge(full_data, final_zip_tax, by = c("year", "month", "fips"), all.x = TRUE)
rm(final_zip_tax)

# Generating an indicator for Amazon and Overstock (Zappos became a subsidiary of Amazon on July 22, 2009)
full_data[, "amazon" := ifelse(domain_name == "amazon.com", 1L, 0L)]
full_data[domain_name == "zappos.com" & event_date >= as.Date("2009-07-22"), "amazon" := 1L]
full_data[, "overstock" := ifelse(domain_name == "overstock.com", 1L, 0L)]

# Generating an indicator for whether Amazon/Overstock collects sales tax in the state
full_data[, "amazon_collect" := ifelse(event_date >= AmazonCollected, 1L, 0L)]
full_data[is.na(amazon_collect), "amazon_collect" := 0L]
full_data[, "overstock_collect" := ifelse(event_date >= OverstockCollected, 1L, 0L)]
full_data[is.na(overstock_collect), "overstock_collect" := 0L]

# Converting "Offline" indicator to a measure of whether tax is collected
full_data[, "taxed_nonamazon" := ifelse(Offline == "Yes", 1L, 0L)]
full_data[overstock == 1, "taxed_nonamazon" := overstock * overstock_collect]
full_data[, "nontaxed_nonamazon" := ifelse(taxed_nonamazon == 0 & amazon == 0, 1L, 0L)]

# Adding month counter to get month fixed effects for later
full_data[, "month_counter" := (year - 2006L) * 12L + month]

# Generating border county indicator
full_data[, "border_county" := ifelse(min_adj_tax == 0 & sales_tax > 0, 1L, 0L)]

# Generating tax diff and tax ratio
full_data[, c("tax_diff") := .(sales_tax - min_adj_tax)]
full_data[, c("LegallyRequired", "AmazonCollected", "OverstockCollected", "zip_code") := NULL]

# Saving
fwrite(full_data, file = "./code/0_data/Clean/comScoreTransactionReg.csv")
