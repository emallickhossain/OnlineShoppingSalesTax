# This replicates Katja's regressions
# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
library(biglm)
full_data <- fread("./code/0_data/Clean/comScoreTransactionReg.csv")
full_data[, "amazon_collect_year" := min(amazon_collect), by = .(year, state)]

# Formula to iterate over regression specifications
runReg <- function(x, y, data) {
  eq <- formula(paste0(y, " ~ ", x, "| ",
                       "size + age + income + children + race + hispanic + month_counter + fips ",
                       "+ prod_category_id | 0 | state"))
  output <- felm(eq, data = data)
  return(output)
}

x <- c("sales_tax",
       "sales_tax + sales_tax : amazon_collect",
       "sales_tax + sales_tax : amazon_collect + tax_diff",
       "sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect")

# Amazon probability table
y <- "amazon"
allCounties <- map(x, runReg, data = full_data, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = full_data[border_county == 1], y = y)
stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          notes.align = "l", digits = 2,
          title = "Probability of Amazon Purchase",
          label = "tab:amazonProb",
          column.labels = c("All Counties", "Border County"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Probability of Purchase",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("Product Cat. FE", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.")
,
          out = "./tables/prob_amazon_expenditures_hh.tex")

# Katja probability
reg3 <- felm(amazon ~ sales_tax * amazon_collect_year |
               income + race + age + year + fips + prod_category_id |
               0 | fips + year, data = full_data)

# Non-Amazon probability table
y <- "taxed_nonamazon"
allCounties <- map(x, runReg, data = full_data, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = full_data[border_county == 1], y = y)
stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          notes.align = "l", digits = 2,
          title = "Probability of Taxed Non-Amazon Purchase",
          label = "tab:taxed_nonamazonProb",
          column.labels = c("All Counties", "Border County"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Probability of Purchase",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("Product Cat. FE", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.")
,
          out = "./tables/prob_nonamazon_taxed_expenditures_hh.tex")

# Non-taxed non-Amazon purchases
y <- "nontaxed_nonamazon"
allCounties <- map(x, runReg, data = full_data, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = full_data[border_county == 1], y = y)
stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          notes.align = "l", digits = 2,
          title = "Probability of Non-Taxed Non-Amazon Purchase",
          label = "tab:nontaxed_nonamazonProb",
          column.labels = c("All Counties", "Border County"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Probability of Purchase",
          covariate.labels = c("Tax",
                               "Tax * Collect",
                               "Tax Diff",
                               "Tax Diff * Collect"),
          order = c(1, 3, 2, 4),
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("Product Cat. FE", "Y", "Y", "Y", "Y", "Y"),
                           c("County FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Month-Year FE", "Y", "Y", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the county level.",
          out = "./tables/prob_nonamazon_nontaxed_expenditures_hh.tex")

# LOGIT ROBUSTNESS -------------------------------------------------------------
reg1 <- bigglm(data = full_data, amazon ~ log(1 + sales_tax) +
              income + race + age + children + month_counter + fips + prod_category_id,
            family = binomial(logit))
reg2 <- glm(data = full_data, amazon ~ log(1 + sales_tax) +
              log(1 + sales_tax) : amazon_collect +
              income + race + age + children + month_counter + fips + prod_category_id,
            family = binomial(link = "logit"))
reg3 <- glm(data = full_data, amazon ~ log(1 + sales_tax) +
              log(1 + sales_tax) : amazon_collect + tax_diff +
              income + race + age + children + month_counter + fips + prod_category_id,
            family = binomial(link = "logit"))
reg3 <- glm(data = full_data, amazon ~ log(1 + sales_tax) +
              log(1 + sales_tax) : amazon_collect + tax_diff + tax_diff : amazon_collect +
              income + race + age + children + month_counter + fips + prod_category_id,
            family = binomial(link = "logit"))
