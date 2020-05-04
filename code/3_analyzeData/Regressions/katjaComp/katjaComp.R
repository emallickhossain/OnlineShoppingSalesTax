# Check Katja regressions
library(data.table)
library(lfe)
library(readxl)
library(maps)
library(stargazer)

amazon <- c("amazon.com", "amazon.ca", "amazon.co.uk", "amazon.de", "amazon.fr")

# Loading data
transactions <- fread("./code/0_data/katjaTransactions.csv")
transactions[, "year" := year(event_date)]
panel <- fread("./code/0_data/Clean/DemographicsClean.csv")
browse <- fread("./code/0_data/Clean/fullTransactionBrowsing.csv")

# Getting number of months active
monthBrowse <- browse[, .(duration = sum(monthlyDuration)),
                      by = .(machine_id, year, month)]
monthBrowse <- monthBrowse[, .(nMonths = .N), by = .(machine_id, year)]

########## Analysis for Katja transactions
# Calculating annual spending and number of transactions
annualTrans <- transactions[, .(total = sum(prod_totprice),
                                nTrans = .N),
                            by = .(machine_id, year)]
amazonSpend <- transactions[domain_name %in% amazon, .(amazon = sum(prod_totprice)),
                            by = .(machine_id, year)]
annualTrans <- merge(annualTrans, panel, by = c("machine_id", "year"), all.y = TRUE)
annualTrans <- merge(annualTrans, amazonSpend, by = c("machine_id", "year"), all.x = TRUE)
annualTrans[is.na(total), "total" := 0]
annualTrans[is.na(nTrans), "nTrans" := 0]
annualTrans[is.na(amazon), "amazon" := 0]

# Adjusting annual spending by number of months active in sample
annualTrans <- merge(annualTrans, monthBrowse, by = c("machine_id", "year"))
annualTrans[, ':=' (adjTotal = total / nMonths * 12,
                    adjAmazon = amazon / nMonths * 12)]

annualTrans[, .(adjSpend = mean(adjTotal),
                nTrans = mean(nTrans),
                offline = sum(nTrans == 0) / .N,
                amazon = mean(adjAmazon)), keyby = year]

# Getting annual county data and merging with sales tax collection
countySpend <- annualTrans[, .(amazon = mean(amazon),
                               adjAmazon = mean(adjAmazon)), by = .(fips, year)]
countySpend[, "state" := as.integer(substr(fips, 1, 2))]
countySpend <- merge(countySpend, unique(state.fips[, c("fips", "abb")]),
                     by.x = "state", by.y = "fips")
countySpend[, "state" := NULL]
setnames(countySpend, "abb", "state")

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, .(state, AmazonCollected)]
amazon_tax[, c("AmazonCollected") := .(as.Date(AmazonCollected))]
countySpend <- merge(countySpend, amazon_tax, by = "state")

# Generating an indicator for whether Amazon/Overstock collects sales tax in the state
countySpend[, "amazon_collect" := ifelse(year >= year(AmazonCollected), 1L, 0L)]
countySpend[is.na(amazon_collect), "amazon_collect" := 0L]

reg1a <- felm(log(1 + adjAmazon) ~ as.factor(amazon_collect) | fips + year,
             data = countySpend)
reg2a <- felm(log(1 + adjAmazon) ~ as.factor(amazon_collect) | fips + year,
             data = countySpend[adjAmazon > 0])
reg3a <- felm(log(1 + adjAmazon) ~ as.factor(amazon_collect) | fips + year,
             data = countySpend[year <= 2013])
reg4a <- felm(log(1 + adjAmazon) ~ as.factor(amazon_collect) | fips + year,
             data = countySpend[year <= 2013 & adjAmazon > 0])
stargazer(reg1a, reg2a, reg3a, reg4a, type = "text")

reg1b <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend)
reg2b <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[amazon > 0])
reg3b <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[year <= 2013])
reg4b <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[year <= 2013 & amazon > 0])
stargazer(reg1b, reg2b, reg3b, reg4b, type = "text")


#### Running on the regression data
transact <- fread("./code/0_data/Clean/comScoreTransactionReg.csv")[amazon == 1]
# transact <- fread("./code/0_data/katjaTransactions.csv")[domain_name %in% amazon]
# transact[, "year" := year(event_date)]

annualTrans <- transact[, .(amazon = sum(prod_totprice)), by = .(machine_id, year)]
full_data <- merge(annualTrans, panel, by = c("machine_id", "year"), all.y = TRUE)
full_data[is.na(amazon), "amazon" := 0]

# Getting annual county data and merging with sales tax collection
countySpend <- full_data[, .(amazon = mean(amazon)), by = .(fips, year)]
countySpend[, "state" := as.integer(substr(fips, 1, 2))]
countySpend <- merge(countySpend, unique(state.fips[, c("fips", "abb")]),
                     by.x = "state", by.y = "fips")
countySpend[, "state" := NULL]
setnames(countySpend, "abb", "state")

# Merging with Amazon law spreadsheet
amazon_tax <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, .(state, AmazonCollected)]
amazon_tax[, c("AmazonCollected") := .(as.Date(AmazonCollected))]
countySpend <- merge(countySpend, amazon_tax, by = "state")

# Generating an indicator for whether Amazon/Overstock collects sales tax in the state
countySpend[, "amazon_collect" := ifelse(year >= year(AmazonCollected), 1L, 0L)]
countySpend[is.na(amazon_collect), "amazon_collect" := 0L]

reg1c <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend)
reg2c <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[amazon > 0])
reg3c <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[year <= 2013])
reg4c <- felm(log(1 + amazon) ~ as.factor(amazon_collect) | fips + year,
              data = countySpend[year <= 2013 & amazon > 0])
stargazer(reg1c, reg2c, reg3c, reg4c, type = "text")
