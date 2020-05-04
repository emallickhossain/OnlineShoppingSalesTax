# ------------------ ABOUT -----------------------------------------------------
# This file takes the comScore Transaction and Demographics data and combines it
# with the Thomson Reuters tax data, the Census county adjacency file, and
# the Missouri Census Data Center Geographic Correspondence Engine to obtain
# the final data set on which I conduct my analysis.

# ----------------- CODE -------------------------------------------------------
library(data.table)
library(readxl)

# Load transaction and demographic data
demographics <- fread("./code/0_data/Clean/DemographicsClean.csv")
full_transaction_browsing <- fread("./code/0_data/Clean/fullTransactionBrowsing.csv")
full_transaction_browsing[, "date" := as.Date(paste(year, month, "01", sep = "-"))]

# Adding online/offline indicator
online_offline <- setDT(read_excel("./code/0_data/onlineOffline.xls"))
online_offline[, c("Notes", "totalSales") := NULL]
full_transaction_browsing <- merge(full_transaction_browsing, online_offline,
                                   by = "domain_name", all.x = TRUE)
full_transaction_browsing[is.na(Offline), "Offline" := "No"]

# Merging
full_data <- merge(demographics, full_transaction_browsing, by = c("year", "machine_id"))
rm(full_transaction_browsing, demographics, online_offline)

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, .(state, LegallyRequired, AmazonCollected, OverstockCollected)]
amazon_tax[, c("LegallyRequired", "AmazonCollected", "OverstockCollected") :=
             .(as.Date(LegallyRequired), as.Date(AmazonCollected),
               as.Date(OverstockCollected))]
full_data <- merge(full_data, amazon_tax, by = "state")
rm(amazon_tax)

# Merging with tax rates (no tax info is available outside of 2006-2016)
final_zip_tax <- fread("./code/0_data/Clean/zip_tax_min.csv")
full_data <- merge(full_data, final_zip_tax, by = c("year", "month", "fips"), all.x = TRUE)
rm(final_zip_tax)

# Generating an indicator for Amazon (Zappos became a subsidiary of Amazon on July 22, 2009)
full_data[, "amazon" := ifelse(domain_name == "amazon.com", 1L, 0L)]
full_data[domain_name == "zappos.com" & date >= as.Date("2009-08-01"), "amazon" := 1L]

# Generating an indicator for Overstock
full_data[, "overstock" := ifelse(domain_name == "overstock.com", 1L, 0L)]

# Generating an indicator for whether Amazon collects sales tax in the state
full_data[, "amazon_collect" := ifelse(date >= AmazonCollected, 1L, 0L)]
full_data[is.na(amazon_collect), "amazon_collect" := 0L]

# Generating an indicator for whether Overstock collects sales tax in the state
full_data[, "overstock_collect" := ifelse(date >= OverstockCollected, 1L, 0L)]
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

# Cleaning up to save space
full_data[, c("zip_code", "domain_id", "LegallyRequired",
              "AmazonCollected", "OverstockCollected") := NULL]

# Saving
fwrite(full_data, file = "./code/0_data/Clean/comScoreBrowsingRegData.csv")
