# Gets summary stats table for comScore data
# Restricting to households that buy something on Amazon and spend less than $2k/year
library(data.table)
library(stargazer)
library(forcats)
full_data <- fread("./code/0_data/Clean/comScoreTransactionReg.csv")
nrow(full_data)

# Restricting to households that buy on Amazon
full_data[age == 1, "ageMid" := 19]
full_data[age == 2, "ageMid" := 22.5]
full_data[age == 3, "ageMid" := 27]
full_data[age == 4, "ageMid" := 32]
full_data[age == 5, "ageMid" := 37]
full_data[age == 6, "ageMid" := 42]
full_data[age == 7, "ageMid" := 47]
full_data[age == 8, "ageMid" := 52]
full_data[age == 9, "ageMid" := 57]
full_data[age == 10, "ageMid" := 62]
full_data[age == 11, "ageMid" := 65]
full_data[income == 1, "incomeMid" := 7.5]
full_data[income == 2, "incomeMid" := 20]
full_data[income == 3, "incomeMid" := 30]
full_data[income == 4, "incomeMid" := 42.5]
full_data[income == 5, "incomeMid" := 62.5]
full_data[income == 6, "incomeMid" := 87.5]
full_data[income == 7, "incomeMid" := 100]
full_data[income == 11, "incomeMid" := 12.5]
full_data[income == 12, "incomeMid" := 32.5]
full_data[income == 13, "incomeMid" := 50]
full_data[income == 14, "incomeMid" := 67.5]
full_data[income == 15, "incomeMid" := 87.5]
full_data[income >= 16, "incomeMid" := 100]

# Demographics
summaryStats <- unique(full_data[, .(machine_id, size, ageMid, incomeMid, children,
                                     hispanic, sales_tax)])[, "machine_id" := NULL]
stargazer(summaryStats, no.space = TRUE, type = "text",
          title = "comScore Panel Summary Statistics",
          label = "tab:comScorePanel",
          covariate.labels = c("Household Size", "Age", "Income", "Child Present",
                               "Hispanic", "Sales Tax", "Border County"),
          digits = 2,
          summary.stat = c("mean", "median", "sd", "min", "max"),
          out = "./tables/comScoreSummaryStatsPanel.tex")
nrow(summaryStats)

# Transactions
summaryStats <- full_data[, .(prod_totprice_real, sales_tax, amazon, taxed_nonamazon, nontaxed_nonamazon)]
stargazer(summaryStats, no.space = TRUE, type = "text",
          title = "comScore Transaction Summary Statistics",
          label = "tab:comScoreSummary",
          covariate.labels = c("Real Product Price", "Sales Tax", "Amazon Purchase",
                               "Offline Amazon Competitor", "Online Amazon Competitor"),
          digits = 2,
          summary.stat = c("mean", "median", "sd", "min", "max"),
          out = "./tables/comScoreSummaryStats.tex")
nrow(summaryStats)

# Browsing summary stats
# Removing non-sales domains and extreme browsing durations
browse <- fread("./code/0_data/Clean/comScoreBrowsingRegData.csv")[monthlyDuration > 0]

# Aggregating browsing to the household-month level
browse[, ':=' (amazon_min = amazon * monthlyDuration,
               nontax_nonamazon_min = nontaxed_nonamazon * monthlyDuration,
               tax_nonamazon_min = taxed_nonamazon * monthlyDuration)]
monthlyBrowse <- browse[machine_id %in% unique(full_data$machine_id),
                        .(amazon_min = sum(amazon_min),
                          nontax_nonamazon_min = sum(nontax_nonamazon_min),
                          tax_nonamazon_min = sum(tax_nonamazon_min)),
                        keyby = .(machine_id, year, month)]

# Removing top 1% of browsing to address outliers
monthlyBrowse[, "total" := amazon_min + nontax_nonamazon_min + tax_nonamazon_min]
top1Browse <- quantile(monthlyBrowse$total, 0.999)
bigBrowse <- unique(monthlyBrowse[total >= top1Browse]$machine_id)
monthlyBrowse <- monthlyBrowse[!machine_id %in% bigBrowse]

# Calculating panelist tenure
nMonths <- monthlyBrowse[, .(months = .N), by = machine_id]
quantile(nMonths$months)

# Browsing
summaryStats <- monthlyBrowse[, .(total, amazon_min, nontax_nonamazon_min, tax_nonamazon_min)]
stargazer(summaryStats, no.space = TRUE, type = "text",
          title = "comScore Browsing Summary Statistics (Minutes)",
          label = "tab:comScoreBrowsing",
          covariate.labels = c("Total", "Amazon", "Untaxed Competitor", "Taxed Competitor"),
          digits = 2,
          summary.stat = c("mean", "median", "sd", "min", "max"),
          out = "./tables/comScoreSummaryStatsBrowsing.tex")
nrow(summaryStats)
