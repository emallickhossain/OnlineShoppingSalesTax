# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
full_data <- fread("./code/0_data/Clean/comScoreTransactionReg.csv")

# ------------------------ Household-Month Spending ----------------------------
# I collapse the data to household transaction observations. There is an issue
# with California because it adopted its Amazon law in the middle of a month, so
# there are two observations in September 2012 when I collapse this way. Just
# to be safe, I drop these observations.
# Aggregate by year, month, household
pretax_spending <- full_data[state != "CA" & year != 2012 & month != 9,
                             .(log_spending = log(sum(prod_totprice_real))),
                             keyby = .(month_counter, fips, machine_id, state,
                                       size, age, income, children, race, hispanic,
                                       sales_tax, min_adj_tax, amazon_collect,
                                       tax_diff, amazon, nontaxed_nonamazon,
                                       taxed_nonamazon)]

rm(full_data)

# Formula to iterate over regression specifications
runReg <- function(x, y, data) {
  eq <- formula(paste0(y, " ~ ", x, "| ",
                       "size + age + income + children + race + hispanic + month_counter + fips | 0 | fips"))
                       #"| 0 | state : month_counter")) # Why did I not cluster? Because standard errors get too big.
  output <- felm(eq, data = data)
  return(output)
}

x <- c("sales_tax",
       "sales_tax + sales_tax : amazon_collect",
       "sales_tax + sales_tax : amazon_collect + tax_diff",
       "sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect")
y <- "log_spending"

# Amazon Expenditures
allCounties <- map(x, runReg, data = pretax_spending[amazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = pretax_spending[min_adj_tax == 0 & sales_tax != 0 & amazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Amazon Expenditures",
          label = "tab:amazonExp",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l")
,
          out = "./tables/amazon_expenditures_hh.tex")

# Taxed Non-Amazon Expenditures
allCounties <- map(x, runReg, data = pretax_spending[taxed_nonamazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = pretax_spending[min_adj_tax == 0 & sales_tax != 0 & taxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Taxed Non-Amazon Expenditures",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/taxed_nonamazon_expenditures_hh.tex")

# Non-Taxed Non-Amazon Expenditures
allCounties <- map(x, runReg, data = pretax_spending[nontaxed_nonamazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = pretax_spending[min_adj_tax == 0 & sales_tax != 0 & nontaxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Non-Taxed Non-Amazon Expenditures",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/nontaxed_nonamazon_expenditures_hh.tex")

# Total Expenditures
allCounties <- map(x, runReg, data = pretax_spending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = pretax_spending[min_adj_tax == 0 & sales_tax != 0], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Total Online Expenditures",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l",
          out = "./tables/total_expenditures_hh.tex")
