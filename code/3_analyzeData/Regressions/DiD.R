# Running regressions to see tax elasticity
# Only keeping households that spend on Amazon and less than $2k/year
library(data.table)
library(lfe)
library(purrr)
library(stargazer)

# Loading data
transact <- fread("./code/0_data/Clean/comScoreTransactionReg.csv",
                  select = c("machine_id", "year", "month", "prod_totprice_real",
                             "amazon", "taxed_nonamazon", "nontaxed_nonamazon"))
browse <- fread("./code/0_data/Clean/comScoreBrowsingRegData.csv")[monthlyDuration > 0]

# Aggregating browsing to the household-month level
browse[, ':=' (amazon_min = amazon * monthlyDuration,
               nontax_nonamazon_min = nontaxed_nonamazon * monthlyDuration,
               tax_nonamazon_min = taxed_nonamazon * monthlyDuration)]
monthlyBrowse <- browse[, .(amazon_min = sum(amazon_min),
                            nontax_nonamazon_min = sum(nontax_nonamazon_min),
                            tax_nonamazon_min = sum(tax_nonamazon_min)),
                        keyby = .(machine_id, year, month, fips, state,
                                  sales_tax, min_adj_tax, amazon_collect,
                                  border_county, tax_diff, size, age, income,
                                  children, race, hispanic)]

# Removing top 0.1% of browsing to address outliers who browse > 7 hours / day
monthlyBrowse[, "total" := amazon_min + nontax_nonamazon_min + tax_nonamazon_min]
top1Browse <- quantile(monthlyBrowse$total, 0.999)
bigBrowse <- unique(monthlyBrowse[total >= top1Browse]$machine_id)
monthlyBrowse <- monthlyBrowse[!machine_id %in% bigBrowse]

# Aggregating spending to the household-month level
transact[, ':=' (amazon_spend = amazon * prod_totprice_real,
                 nontax_nonamazon_spend = nontaxed_nonamazon * prod_totprice_real,
                 tax_nonamazon_spend = taxed_nonamazon * prod_totprice_real)]
monthlySpend <- transact[, .(amazon_spend = sum(amazon_spend),
                            nontax_nonamazon_spend = sum(nontax_nonamazon_spend),
                            tax_nonamazon_spend = sum(tax_nonamazon_spend)),
                        keyby = .(machine_id, year, month)]

# Merging together
fullData <- merge(monthlyBrowse, monthlySpend, by = c("machine_id", "year", "month"),
                  all.x = TRUE)
fullData[is.na(amazon_spend), "amazon_spend" := 0]
fullData[is.na(nontax_nonamazon_spend), "nontax_nonamazon_spend" := 0]
fullData[is.na(tax_nonamazon_spend), "tax_nonamazon_spend" := 0]
rm(browse, transact, monthlyBrowse, monthlySpend)

# Getting households that have any Amazon spending (only 17% buy from Amazon)
onlineSpend <- fullData[, .(amazon = sum(amazon_spend)), by = .(machine_id)]
amazonID <- onlineSpend[amazon > 0 & amazon < 2000]$machine_id
length(amazonID) / uniqueN(fullData$machine_id)
amazonSample <- fullData[machine_id %in% amazonID]
amazonSample[, "date" := as.Date(paste(year, month, "01", sep = "-"))]
fullData[, "date" := as.Date(paste(year, month, "01", sep = "-"))]

# Checking how many households actually experience a switch in tax collection
switchers <- amazonSample[, uniqueN(amazon_collect), by = machine_id]
uniqueN(switchers[V1 > 1]$machine_id)
uniqueN(amazonSample$machine_id)

# Getting Houde, Newberry, Seim annual spending
annualcomScore <- amazonSample[, .(sales_tax = mean(sales_tax),
                                   amazon_spend = mean(amazon_spend) * 12,
                                   amazon_collect = max(amazon_collect)),
                               by = .(year, fips)]
annualcomScore2 <- fullData[, .(sales_tax = mean(sales_tax),
                                   amazon_spend = mean(amazon_spend) * 12,
                                   amazon_collect = max(amazon_collect)),
                               by = .(year, fips)]
reg1 <- felm(log(1 + amazon_spend) ~ as.factor(amazon_collect) | fips + year,
             data = annualcomScore[year <= 2013])
reg2 <- felm(log(1 + amazon_spend) ~ as.factor(amazon_collect) | fips + year,
             data = annualcomScore[year <= 2013 & amazon_spend > 0])
reg3 <- felm(log(1 + amazon_spend) ~ as.factor(amazon_collect) | fips + year,
             data = annualcomScore2[year <= 2013])
reg4 <- felm(log(1 + amazon_spend) ~ as.factor(amazon_collect) | fips + year,
             data = annualcomScore2[year <= 2013 & amazon_spend > 0])
reg4a <- felm(amazon_spend ~ as.factor(amazon_collect) | fips + year,
             data = annualcomScore2[year <= 2013 & amazon_spend > 0])
stargazer(reg3, reg4, reg1, reg2, type = "text",
          add.lines = list(c("County FE", rep("Y", 4)),
                           c("Year FE", rep("Y", 4)),
                           c("Positive County-Years", "N", "Y", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.separate = c(2, 2),
          column.labels = c("Full Sample", "Amazon Sample"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3)

############## SPENDING BEHAVIOR ##############################################
# Running regressions on Amazon collection indicator
# NOTES: 1. Given household fixed effect, sales tax variable is only identified off
# of changes in tax rates which are relatively rare and small. Collection coefficient
# does not meaningfully change with its inclusion or exclusion
# 2. Use levels because using logs is sensitive to households that have 0 pre-tax
# spending and then positive spending after the tax is collected
# 3. I use the full sample of data instead of only households that have shopped
# on Amazon. This slightly increases Amazon elasticity estimates, but it also
# provides more power to identify non-Amazon shopping.
reg1a <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg2a <- felm(nontax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg3a <- felm(tax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)

reg1b <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = fullData)
reg2b <- felm(nontax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = fullData)
reg3b <- felm(tax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = fullData)

# Running regressions on sales tax treatment indicator
reg4a <- felm(amazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
             data = amazonSample)
reg5a <- felm(nontax_nonamazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
             data = amazonSample)
reg6a <- felm(tax_nonamazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
             data = amazonSample)

reg4b <- felm(amazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = fullData)
reg5b <- felm(nontax_nonamazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = fullData)
reg6b <- felm(tax_nonamazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = fullData)

# Running regressions on border counties
reg7 <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
             data = fullData[border_county == 1])
reg8 <- felm(nontax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
             data = fullData[border_county == 1])
reg9 <- felm(tax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
              data = fullData[border_county == 1])

# Getting mean spending and tax rates
uncond_mean_amazon <- round(mean(fullData$amazon_spend), digits = 2)
uncond_mean_nontax_nonamazon <- round(mean(fullData$nontax_nonamazon_spend), digits = 2)
uncond_mean_tax_nonamazon <- round(mean(fullData$tax_nonamazon_spend), digits = 2)

uncond_mean_amazon_brd <- round(mean(amazonSample[border_county == 1]$amazon_spend), digits = 2)
uncond_mean_nontax_nonamazon_brd <- round(mean(amazonSample[border_county == 1]$nontax_nonamazon_spend), digits = 2)
uncond_mean_tax_nonamazon_brd <- round(mean(amazonSample[border_county == 1]$tax_nonamazon_spend), digits = 2)

mean_tax <- round(mean(fullData$sales_tax), digits = 3)
stargazer(reg1b, reg2b, reg3b, reg4b, reg5b, reg6b, type = "text",
          add.lines = list(c("Household FE", rep("Y", 6)),
                           c("Month-Year FE", rep("Y", 6)),
                           c("Mean Spending",
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon,
                             uncond_mean_tax_nonamazon,
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon,
                             uncond_mean_tax_nonamazon),
                           c("Mean Tax", rep(mean_tax, 6))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Amazon", "Untaxed Sites", "Taxed Sites",
                            "Amazon", "Untaxed Sites", "Taxed Sites"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect", "Collect * Tax Rate"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/spendingDiD.tex")

###### EXAMINING INTENSIVE MARGIN
# Running regressions on Amazon collection indicator
reg1a <- felm(log(1 + amazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[amazon_spend > 0])
reg2a <- felm(log(1 + nontax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[nontax_nonamazon_spend > 0])
reg3a <- felm(log(1 + tax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[tax_nonamazon_spend > 0])

# Running regressions on sales tax treatment indicator
reg4a <- felm(log(1 + amazon_spend) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[amazon_spend > 0])

# Getting mean spending and tax rates
uncond_mean_amazon <- round(mean(amazonSample[amazon_spend > 0]$amazon_spend), digits = 2)
uncond_mean_nontax_nonamazon <- round(mean(amazonSample[nontax_nonamazon_spend > 0]$nontax_nonamazon_spend), digits = 2)
uncond_mean_tax_nonamazon <- round(mean(amazonSample[tax_nonamazon_spend > 0]$tax_nonamazon_spend), digits = 2)

mean_tax <- round(mean(amazonSample$sales_tax), digits = 3)
stargazer(reg1a, reg2a, reg3a, reg4a, type = "text",
          add.lines = list(c("Household FE", rep("Y", 4)),
                           c("Month-Year FE", rep("Y", 4)),
                           c("Mean Spending",
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon,
                             uncond_mean_tax_nonamazon, uncond_mean_amazon),
                           c("Mean Tax", rep(mean_tax, 4))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect", "Collect * Tax Rate"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3)

###### ROBUSTNESS (ONLY 2011-2015 TO MATCH BAUGH, BEN-DAVID, PARK)
BBP <- amazonSample[year >= 2011 & year <= 2015]

reg1bbp_log <- felm(log(1 + amazon_spend) ~ amazon_collect | machine_id + date,
             data = BBP)
reg2bbp_log <- felm(log(1 + nontax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = BBP)
reg3bbp_log <- felm(log(1 + tax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = BBP)
reg4bbp_log <- felm(log(1 + amazon_spend) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
             data = BBP)
reg7bbp_log <- felm(log(1 + amazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
             data = BBP[border_county == 1])

reg1bbp_lev <- felm(amazon_spend ~ amazon_collect | machine_id + date,
                    data = BBP)
reg2bbp_lev <- felm(nontax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                data = BBP)
reg3bbp_lev <- felm(tax_nonamazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                data = BBP)
reg4bbp_lev <- felm(amazon_spend ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
                data = BBP)
reg7bbp_lev <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                data = BBP[border_county == 1])

# Getting mean spending and tax rates
uncond_mean_amazon <- round(mean(BBP$amazon_spend), digits = 2)
uncond_mean_nontax_nonamazon <- round(mean(BBP$nontax_nonamazon_spend), digits = 2)
uncond_mean_tax_nonamazon <- round(mean(BBP$tax_nonamazon_spend), digits = 2)
uncond_mean_amazon_brd <- round(mean(BBP[border_county == 1]$amazon_spend), digits = 2)
mean_tax <- round(mean(BBP$sales_tax), digits = 3)

stargazer(reg1bbp_lev, reg2bbp_lev, reg3bbp_lev, reg4bbp_lev, reg7bbp_lev, type = "text",
          add.lines = list(c("Household FE", rep("Y", 5)),
                           c("Month-Year FE", rep("Y", 5)),
                           c("Mean Spending",
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon, uncond_mean_tax_nonamazon,
                             uncond_mean_amazon, uncond_mean_amazon_brd),
                           c("Mean Tax", rep(mean_tax, 4), 0)),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect", "Collect * Tax Rate"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3)

###### ROBUSTNESS (CHECKING BY AGE)
# <35, 35-54, >=55
regYoung <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                 data = fullData[age < 5])
regMiddle <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                  data = fullData[age %in% 5:8])
regOld <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
               data = fullData[age > 8])
uncond_mean_young <- round(mean(fullData[age < 5]$amazon_spend), digits = 2)
uncond_mean_middle <- round(mean(fullData[age %in% 5:8]$amazon_spend), digits = 2)
uncond_mean_old <- round(mean(fullData[age > 8]$amazon_spend), digits = 2)
uncond_mean_young_tax <- round(mean(fullData[age < 5]$sales_tax), digits = 2)
uncond_mean_middle_tax <- round(mean(fullData[age %in% 5:8]$sales_tax), digits = 2)
uncond_mean_old_tax <- round(mean(fullData[age > 8]$sales_tax), digits = 2)
stargazer(regYoung, regMiddle, regOld, type = "text",
          add.lines = list(c("Household FE", rep("Y", 3)),
                           c("Month-Year FE", rep("Y", 3)),
                           c("Mean Spending",
                             uncond_mean_young, uncond_mean_middle, uncond_mean_old),
                           c("Mean Tax", uncond_mean_young_tax,
                             uncond_mean_middle_tax, uncond_mean_old_tax)),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "Real Expenditures", dep.var.labels.include = FALSE,
          column.labels = c("<35", "35-54", ">54"),
          covariate.labels = c("Collect"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/appendixSpendingAge.tex")

###### ROBUSTNESS (CHECKING BY INCOME)
# <25K, 25-75k, >75k
regPoor <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                 data = fullData[income %in% c(1, 2, 11)])
regMidClass <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
                  data = fullData[income %in% c(3:5, 12:14)])
regRich <- felm(amazon_spend ~ amazon_collect | machine_id + date | 0 | state,
               data = fullData[income %in% c(6:7, 16:18)])
uncond_mean_poor <- round(mean(fullData[income %in% c(1, 2, 11)]$amazon_spend), digits = 2)
uncond_mean_midclass <- round(mean(fullData[income %in% c(3:5, 12:14)]$amazon_spend), digits = 2)
uncond_mean_rich <- round(mean(fullData[income %in% c(6:7, 16:18)]$amazon_spend), digits = 2)
uncond_mean_poor_tax <- round(mean(fullData[income %in% c(1, 2, 11)]$sales_tax), digits = 2)
uncond_mean_midclass_tax <- round(mean(fullData[income %in% c(3:5, 12:14)]$sales_tax), digits = 2)
uncond_mean_rich_tax <- round(mean(fullData[income %in% c(6:7, 16:18)]$sales_tax), digits = 2)
stargazer(regPoor, regMidClass, regRich, type = "text",
          add.lines = list(c("Household FE", rep("Y", 3)),
                           c("Month-Year FE", rep("Y", 3)),
                           c("Mean Spending",
                             uncond_mean_poor, uncond_mean_midclass, uncond_mean_rich),
                           c("Mean Tax", uncond_mean_poor_tax,
                             uncond_mean_midclass_tax, uncond_mean_rich_tax)),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "Real Expenditures", dep.var.labels.include = FALSE,
          column.labels = c("<25k", "25-75k", ">75k"),
          covariate.labels = c("Collect"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/appendixSpendingIncome.tex")

###### ROBUSTNESS (ONLY SWITCHERS) #######
switchers <- amazonSample[, uniqueN(amazon_collect), by = machine_id][V1 > 1]$machine_id
switchSample <- amazonSample[machine_id %in% switchers]

reg1a <- felm(log(1 + amazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
              data = switchSample)
reg2a <- felm(log(1 + nontax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
              data = switchSample)
reg3a <- felm(log(1 + tax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
              data = switchSample)

reg4a <- felm(log(1 + amazon_spend) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = switchSample)
reg5a <- felm(log(1 + nontax_nonamazon_spend) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = switchSample)
reg6a <- felm(log(1 + tax_nonamazon_spend) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = switchSample)

reg7a <- felm(log(1 + amazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
              data = switchSample[border_county == 1])
reg8a <- felm(log(1 + nontax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
              data = switchSample[border_county == 1])
reg9a <- felm(log(1 + tax_nonamazon_spend) ~ amazon_collect | machine_id + date | 0 | state,
               data = switchSample[border_county == 1])
stargazer(reg1a, reg2a, reg3a, reg4a, reg5a, reg6a, reg7a, reg8a, reg9a, type = "text",
          add.lines = list(c("Household FE", rep("Y", 9)),
                           c("Month-Year FE", rep("Y", 9)),
                           c("Mean Spending",
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon, uncond_mean_tax_nonamazon,
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon, uncond_mean_tax_nonamazon,
                             uncond_mean_amazon_brd, uncond_mean_nontax_nonamazon_brd, uncond_mean_tax_nonamazon_brd),
                           c("Mean Tax", rep(mean_tax, 6), rep(0, 3))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect", "Collect * Tax Rate"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3)

###################### BROWSING BEHAVIOR ######################################
# Examining how browsing behavior changes
# Running regressions on Amazon collection indicator
amazonSample[, "totalBrowse" := amazon_min + nontax_nonamazon_min + tax_nonamazon_min]
fullData[, "totalBrowse" := amazon_min + nontax_nonamazon_min + tax_nonamazon_min]

reg10 <- felm(amazon_min ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg11 <- felm(nontax_nonamazon_min ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg12 <- felm(tax_nonamazon_min ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)

reg10a <- felm(amazon_min ~ amazon_collect | machine_id + date | 0 | state,
               data = fullData)
reg11a <- felm(nontax_nonamazon_min ~ amazon_collect | machine_id + date | 0 | state,
               data = fullData)
reg12a <- felm(tax_nonamazon_min ~ amazon_collect | machine_id + date | 0 | state,
               data = fullData)

# Running regressions on sales tax treatment indicator
reg13 <- felm(amazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg14 <- felm(nontax_nonamazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)
reg15 <- felm(tax_nonamazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample)

reg13a <- felm(amazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
               data = fullData)
reg14a <- felm(nontax_nonamazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
               data = fullData)
reg15a <- felm(tax_nonamazon_min ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
               data = fullData)

# Running regressions on border counties
reg16 <- felm(log(1 + amazon_min) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[border_county == 1])
reg17 <- felm(log(1 + nontax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[border_county == 1])
reg18 <- felm(log(1 + tax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
             data = amazonSample[border_county == 1])

# Getting mean spending and tax rates
uncond_mean_amazon <- round(mean(fullData$amazon_min), digits = 2)
uncond_mean_nontax_nonamazon <- round(mean(fullData$nontax_nonamazon_min), digits = 2)
uncond_mean_tax_nonamazon <- round(mean(fullData$tax_nonamazon_min), digits = 2)

uncond_mean_amazon_brd <- round(mean(amazonSample[border_county == 1]$amazon_min), digits = 2)
uncond_mean_nontax_nonamazon_brd <- round(mean(amazonSample[border_county == 1]$nontax_nonamazon_min), digits = 2)
uncond_mean_tax_nonamazon_brd <- round(mean(amazonSample[border_county == 1]$tax_nonamazon_min), digits = 2)

mean_tax <- round(mean(fullData$sales_tax), digits = 3)

stargazer(reg10a, reg11a, reg12a, reg13a, reg14a, reg15a, type = "text",
          add.lines = list(c("Household FE", rep("Y", 6)),
                           c("Month-Year FE", rep("Y", 6)),
                           c("Mean Browsing (Min)",
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon,
                             uncond_mean_tax_nonamazon,
                             uncond_mean_amazon, uncond_mean_nontax_nonamazon,
                             uncond_mean_tax_nonamazon),
                           c("Mean Tax", rep(mean_tax, 6))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "Minutes Browsed", dep.var.labels.include = FALSE,
          column.labels = c("Amazon", "Untaxed Sites", "Taxed Sites",
                            "Amazon", "Untaxed Sites", "Taxed Sites"),
          covariate.labels = c("Collect", "Collect * Tax Rate"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/browsingDiD.tex")

# Examining how browsing behavior changes, only for months with positive browsing
# Running regressions on Amazon collection indicator
reg19 <- felm(log(1 + amazon_min) ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[amazon_min > 0])
reg20 <- felm(log(1 + nontax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[nontax_nonamazon_min > 0])
reg21 <- felm(log(1 + tax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[tax_nonamazon_min > 0])

# Running regressions on sales tax treatment indicator
reg22 <- felm(log(1 + amazon_min) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[amazon_min > 0])
reg23 <- felm(log(1 + nontax_nonamazon_min) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[nontax_nonamazon_min > 0])
reg24 <- felm(log(1 + tax_nonamazon_min) ~ sales_tax : amazon_collect | machine_id + date | 0 | state,
              data = amazonSample[tax_nonamazon_min > 0])
stargazer(reg19, reg20, reg21, reg22, reg23, reg24, type = "text")


fullData[, "totalBrowse" := amazon_min + nontax_nonamazon_min + tax_nonamazon_min]
fullData[, "date" := as.Date(paste(year, month, "01", sep = "-"))]
reg25 <- felm(log(1 + totalBrowse) ~ sales_tax + sales_tax : amazon_collect | size + age + income + children +
                race + hispanic + fips + date | 0 | fips, data = fullData)
reg26 <- felm(log(1 + totalBrowse) ~ amazon_collect | machine_id + date | 0 | state,
              data = fullData)
reg27 <- felm(log(1 + totalBrowse) ~ amazon_collect : sales_tax | machine_id + date | 0 | state,
              data = fullData)
stargazer(reg25, reg26, reg27, type = "text")

########### ROBUSTNESS BY AGE GROUP #####################33
# <35, 35-54, >=55
regYoung <- felm(log(1 + tax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
                 data = amazonSample[age < 5])
regMiddle <- felm(log(1 + tax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
                  data = amazonSample[age %in% 5:8])
regOld <- felm(log(1 + tax_nonamazon_min) ~ amazon_collect | machine_id + date | 0 | state,
               data = amazonSample[age > 8])
stargazer(regYoung, regMiddle, regOld, type = "text")
