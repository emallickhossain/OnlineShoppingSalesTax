# Runs Regressions for post-tax values as robustness
# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
load("/home/mallick/Desktop/comScore/comScoreTransactionReg.rda")

# Getting post tax expenditures as well. Basket total is the same for all items in a transaction
post_tax <- unique(full_data, by = c("year", "fips", "month", "domain_name", "month_counter",
                                     "state", "zip_code", "machine_id", "domain_id",
                                     "event_date", "income", "age", "race", "children",
                                     "sales_tax", "min_adj_tax", "amazon_collect",
                                     "tax_diff", "amazon"))

# ------------------------ Household-Month Spending ----------------------------
# I collapse the data to household transaction observations. There is an issue
# with California because it adopted its Amazon law in the middle of a month, so
# there are two observations in September 2012 when I collapse this way. Just
# to be safe, I drop these observations.
# Aggregate by year, month, household

posttax_spending <- post_tax[state != "CA" & year != 2012 & month != 9,
                             .(log_spending = log(sum(basket_tot_real))),
                             keyby = .(year, month, month_counter, machine_id,
                                       income, age, race, fips, state, children,
                                       sales_tax, min_adj_tax, amazon_collect,
                                       tax_diff, amazon, nontaxed_nonamazon,
                                       taxed_nonamazon)]
rm(full_data)
rm(post_tax)

# Formula to iterate over regression specifications
runReg <- function(x, y, data) {
  eq <- formula(paste0(y, " ~ ", x, "| ",
                       "factor(income) + factor(race) + factor(age) + factor(children) + ",
                       "factor(month_counter) + factor(fips)"))
  # "| 0 | fips + month_counter")) # Why did I not cluster? Because standard errors get too big.
  output <- felm(eq, data = data)
  return(output)
}

x <- c("log(1 + sales_tax)",
       "log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect",
       "log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect + tax_diff")
y <- "log_spending"

# Amazon Expenditures
allCounties <- map(x, runReg, data = posttax_spending[amazon == 1], y = y)
borderCounties <- runReg("log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect",
                         data = posttax_spending[min_adj_tax == 0 & sales_tax != 0 & amazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Amazon Expenditures (Post-Tax)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(3, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Log(1 + Tax)",
                               "Log(1 + Tax) * Collect",
                               "Tax Diff"),
          order = c(1, 3, 2),
          notes = c("Household race, income, age, and presence of children as well",
                    "as month-year and county fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/amazon_expenditures_hh_posttax.tex")

# Taxed Non-Amazon Expenditures
allCounties <- map(x, runReg, data = posttax_spending[taxed_nonamazon == 1], y = y)
borderCounties <- runReg("log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect",
                         data = posttax_spending[min_adj_tax == 0 & sales_tax != 0 & taxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Taxed Non-Amazon Expenditures (Post-Tax)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(3, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Log(1 + Tax)",
                               "Log(1 + Tax) * Collect",
                               "Tax Diff"),
          order = c(1, 3, 2),
          notes = c("Household race, income, age, and presence of children as well",
                    "as month-year and county fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/taxed_nonamazon_expenditures_hh_posttax.tex")

# Non-Taxed Non-Amazon Expenditures
allCounties <- map(x, runReg, data = posttax_spending[nontaxed_nonamazon == 1], y = y)
borderCounties <- runReg("log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect",
                         data = posttax_spending[min_adj_tax == 0 & sales_tax != 0 & nontaxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Non-Taxed Non-Amazon Expenditures (Post-Tax)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(3, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Log(1 + Tax)",
                               "Log(1 + Tax) * Collect",
                               "Tax Diff"),
          order = c(1, 3, 2),
          notes = c("Household race, income, age, and presence of children as well",
                    "as month-year and county fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/nontaxed_nonamazon_expenditures_hh_posttax.tex")

# Total Expenditures
allCounties <- map(x, runReg, data = posttax_spending, y = y)
borderCounties <- runReg("log(1 + sales_tax) + log(1 + sales_tax) : amazon_collect",
                         data = posttax_spending[min_adj_tax == 0 & sales_tax != 0], y = y)

stargazer(allCounties, borderCounties,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Total Online Expenditures (Post-Tax)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(3, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Expenditures",
          covariate.labels = c("Log(1 + Tax)",
                               "Log(1 + Tax) * Collect",
                               "Tax Diff"),
          order = c(1, 3, 2),
          notes = c("Household race, income, age, and presence of children as well",
                    "as month-year and county fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./tables/total_expenditures_hh_posttax.tex")
