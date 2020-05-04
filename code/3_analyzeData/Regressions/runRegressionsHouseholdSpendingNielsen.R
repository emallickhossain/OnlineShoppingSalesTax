# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
householdSpending <- fread("/scratch/upenn/hossaine/comScore/regressionDataAll.csv")

# Dropping California in September because it implemented its law in the middle of the month
householdSpending <- householdSpending[fips_state_desc != "CA" & panel_year != 2012 & month != 9]

# Formula to iterate over regression specifications
runReg <- function(x, y, data) {
  eq <- formula(paste0(y, " ~ ", x, "| ",
                       "fips + month_counter + household_income + household_size + ",
                       "marital_status + race + hispanic_origin + age + college | 0 | fips"))
  output <- felm(eq, data = data, weights = data$projection_factor)
  return(output)
}

x <- c("sales_tax",
       "sales_tax + sales_tax : amazon_collect",
       "sales_tax + sales_tax : amazon_collect + tax_diff",
       "sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect")

# Online Expenditures
y <- "logOnlineSpendingReal"
allCounties <- map(x, runReg, data = householdSpending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = householdSpending[min_adj_tax == 0 & sales_tax != 0], y = y)
intensive <- runReg("sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect",
                    data = householdSpending[logOnlineSpendingReal > 0], y = y)

stargazer(allCounties, borderCounties, intensive,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Real Online Expenditures",
          label = "tab:nielsenOnline",
          column.labels = c("All Counties", "Border Counties", "Intensive"),
          column.separate = c(4, 1, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l",
          out = "./tables/nielsen_online_expenditures_hh.tex")

# Offline Expenditures
y <- "logOfflineSpendingReal"
allCounties <- map(x, runReg, data = householdSpending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = householdSpending[min_adj_tax == 0 & sales_tax != 0], y = y)
intensive <- runReg("sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect",
                    data = householdSpending[logOnlineSpendingReal > 0], y = y)

stargazer(allCounties, borderCounties, intensive,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Real Offline Expenditures",
          column.labels = c("All Counties", "Border Counties", "Intensive"),
          column.separate = c(4, 1, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l",
          out = "./tables/nielsen_offline_expenditures_hh.tex")

# Total Expenditures
y <- "logTotalSpendingReal"
allCounties <- map(x, runReg, data = householdSpending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = householdSpending[min_adj_tax == 0 & sales_tax != 0], y = y)
intensive <- runReg("sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect",
                    data = householdSpending[logOnlineSpendingReal > 0], y = y)

stargazer(allCounties, borderCounties, intensive,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Real Total Expenditures",
          column.labels = c("All Counties", "Border Counties", "Intensive"),
          column.separate = c(4, 1, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l",
          out = "./tables/nielsen_total_expenditures_hh.tex")

# Total Grocery
y <- "logGrocSpendingReal"
allCounties <- map(x, runReg, data = householdSpending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = householdSpending[min_adj_tax == 0 & sales_tax != 0], y = y)
intensive <- runReg("sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect",
                    data = householdSpending[logOnlineSpendingReal > 0], y = y)

stargazer(allCounties, borderCounties, intensive,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Real Grocery Expenditures",
          column.labels = c("All Counties", "Border Counties", "Intensive"),
          column.separate = c(4, 1, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l")

# Total Non-Grocery
y <- "logNonGrocSpendingReal"
allCounties <- map(x, runReg, data = householdSpending, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = householdSpending[min_adj_tax == 0 & sales_tax != 0], y = y)
intensive <- runReg("sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect",
                    data = householdSpending[logOnlineSpendingReal > 0], y = y)

stargazer(allCounties, borderCounties, intensive,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Real Non-Grocery Expenditures",
          column.labels = c("All Counties", "Border Counties", "Intensive"),
          column.separate = c(4, 1, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          notes.align = "l")
