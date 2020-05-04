# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)

# Loading data and creating online spender subsample (HHs that have made at least
# one purchase online)
householdSpending <- fread("/scratch/upenn/hossaine/comScore/regressionDataAll.csv")
householdSpending[, ':=' (totalOnline_real = onlineGroc_real + onlineNonGroc_real,
                          totalOffline_real = offlineGroc_real + offlineNonGroc_real)]
onlineSub <- householdSpending[, .(online_real = sum(totalOnline_real) > 0), by = household_code]
onlineSub <- onlineSub[online_real == TRUE]$household_code
onlineSpenders <- householdSpending[household_code %in% onlineSub]

# Getting number of households that switch Amazon collection
switchers <- onlineSpenders[, uniqueN(amazon_collect), by = household_code]
uniqueN(switchers[V1 > 1]$household_code)
uniqueN(onlineSpenders$household_code)

# Running DiD on full sample
reg1 <- felm(total_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg2 <- felm(totalOnline_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg3 <- felm(totalOffline_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg6 <- felm(onlineNonGroc_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg7 <- felm(offlineNonGroc_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)

reg1a <- felm(total_real ~ amazon_collect + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg2a <- felm(totalOnline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg3a <- felm(totalOffline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg6a <- felm(onlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)
reg7a <- felm(offlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = householdSpending, weights = householdSpending$projection_factor)

# Calculating mean spending
total_mean <- round(weighted.mean(householdSpending$total_real,
                                  w = householdSpending$projection_factor), digits = 2)
online_mean <- round(weighted.mean(householdSpending$totalOnline_real,
                                   w = householdSpending$projection_factor), digits = 2)
offline_mean <- round(weighted.mean(householdSpending$totalOffline_real,
                                    w = householdSpending$projection_factor), digits = 2)
online_nonGroc_mean <- round(weighted.mean(householdSpending$onlineNonGroc_real,
                                           w = householdSpending$projection_factor), digits = 2)
offline_nonGroc_mean <- round(weighted.mean(householdSpending$offlineNonGroc_real,
                                            w = householdSpending$projection_factor), digits = 2)
mean_tax <- round(weighted.mean(householdSpending$sales_tax,
                                w = householdSpending$projection_factor), digits = 3)

stargazer(reg1a, reg2a, reg3a, reg6a, reg7a, type = "text",
          add.lines = list(c("Household FE", rep("Y", 5)),
                           c("Month-Year FE", rep("Y", 5)),
                           c("Mean Spending",
                             total_mean, online_mean, offline_mean,
                             online_nonGroc_mean, offline_nonGroc_mean),
                           c("Mean Tax", rep(mean_tax, 4), mean_tax)),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Total", "Online", "Offline",
                            "Online Non-Groc", "Offline Non-Groc"),
          dep.var.caption = "Real Spending", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect"),
          keep = c("collect"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/nielsenDiD.tex")

# Running DiD on online shopper sample
reg8 <- felm(total_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg9 <- felm(totalOnline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg10 <- felm(totalOffline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                 household_size + child + college + white + married | 0 | state,
               data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg13 <- felm(onlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                 household_size + child + college + white + married | 0 | state,
               data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg14 <- felm(offlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                 household_size + child + college + white + married | 0 | state,
               data = onlineSpenders, weights = onlineSpenders$projection_factor)

reg8a <- felm(total_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg9a <- felm(totalOnline_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
               household_size + child + college + white + married | 0 | state,
             data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg10a <- felm(totalOffline_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg13a <- felm(onlineNonGroc_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = onlineSpenders, weights = onlineSpenders$projection_factor)
reg14a <- felm(offlineNonGroc_real ~ amazon_collect : sales_tax + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = onlineSpenders, weights = onlineSpenders$projection_factor)

# Running DiD on only months with positive spending
reg15a <- felm(total_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = householdSpending[total_real > 0],
              weights = householdSpending[total_real > 0]$projection_factor)
reg16a <- felm(totalOnline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = householdSpending[totalOnline_real > 0],
              weights = householdSpending[totalOnline_real > 0]$projection_factor)
reg17a <- felm(totalOffline_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = householdSpending[totalOffline_real > 0],
              weights = householdSpending[totalOffline_real > 0]$projection_factor)
reg20a <- felm(onlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = householdSpending[onlineNonGroc_real > 0],
              weights = householdSpending[onlineNonGroc_real > 0]$projection_factor)
reg21a <- felm(offlineNonGroc_real ~ amazon_collect + age | household_code + date + household_income_coarse +
                household_size + child + college + white + married | 0 | state,
              data = householdSpending[offlineNonGroc_real > 0],
              weights = householdSpending[offlineNonGroc_real > 0]$projection_factor)

# Calculating mean spending
total_mean <- round(weighted.mean(householdSpending[total_real > 0]$total_real,
                                  w = householdSpending[total_real > 0]$projection_factor),
                    digits = 2)
online_mean <- round(weighted.mean(householdSpending[totalOnline_real > 0]$totalOnline_real,
                                   w = householdSpending[totalOnline_real > 0]$projection_factor),
                     digits = 2)
offline_mean <- round(weighted.mean(householdSpending[totalOffline_real > 0]$totalOffline_real,
                                    w = householdSpending[totalOffline_real > 0]$projection_factor),
                      digits = 2)
online_nonGroc_mean <- round(weighted.mean(householdSpending[onlineNonGroc_real > 0]$onlineNonGroc_real,
                                           w = householdSpending[onlineNonGroc_real > 0]$projection_factor),
                             digits = 2)
offline_nonGroc_mean <- round(weighted.mean(householdSpending[offlineNonGroc_real > 0]$offlineNonGroc_real,
                                            w = householdSpending[offlineNonGroc_real > 0]$projection_factor),
                              digits = 2)
mean_tax <- round(weighted.mean(householdSpending$sales_tax,
                                w = householdSpending[total_real > 0]$projection_factor),
                  digits = 3)

stargazer(reg15a, reg16a, reg17a, reg20a, reg21a, type = "text",
          add.lines = list(c("Household FE", rep("Y", 5)),
                           c("Month-Year FE", rep("Y", 5)),
                           c("Mean Spending",
                             total_mean, online_mean, offline_mean,
                             online_nonGroc_mean, offline_nonGroc_mean),
                           c("Mean Tax", rep(mean_tax, 4), mean_tax)),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Total", "Online", "Offline",
                            "Online Non-Groc", "Offline Non-Groc"),
          dep.var.caption = "Real Spending", dep.var.labels.include = FALSE,
          covariate.labels = c("Collect"),
          keep = c("collect"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/nielsenDiDConditional.tex")

# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/tables/nielsenDiD.tex /home/mallick/Desktop/Research/OnlineShopping/OnlineShoppingSalesTax/tables/
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/tables/nielsenDiDConditional.tex /home/mallick/Desktop/Research/OnlineShopping/OnlineShoppingSalesTax/tables/
