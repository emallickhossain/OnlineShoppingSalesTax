# Running regressions to see tax elasticity
library(data.table)
library(lfe)
load('/home/mallick/Desktop/comScore/regressionData.rda')
directory <- '/home/mallick/Desktop/comScore/Regression/CountyAmazon/'

# ------------------------ Baugh Et al Regressions -----------------------------
# In this section, I check the results Baugh gets for Amazon transactions.
# I collapse the data to household-month observations.

# Aggregate by year, month, household, and site session (because otherwise
# basket total is duplicated)
transactions <- full_data[, .(tax_exclusive = sum(prod_totprice),
                              tax_inclusive = mean(basket_tot),
                              sales_tax = mean(sales_tax),
                              min_adj_tax = mean(min_adj_tax),
                              lower_adj_tax = mean(lower_adj_tax),
                              tax_diff = mean(sales_tax - min_adj_tax),
                              tax_ratio = mean((1 + sales_tax) /
                                                 (1 + min_adj_tax)),
                              amazon_collect = mean(amazon_collect),
                              amazon = mean(amazon),
                              taxed = mean(taxed)),
                          keyby = .(year, month, month_counter, machine_id,
                                    household_income_coarse,
                                    hoh_oldest_age_coarse, racial_background,
                                    fips, state, site_session_id, domain_name)]
setnames(transactions, c('household_income_coarse', 'hoh_oldest_age_coarse',
                         'racial_background'), c('income', 'age', 'race'))

# Baugh data divided by Amazon and non-Amazon transactions
baugh <- transactions[, .(tax_exclusive = log(sum(tax_exclusive)),
                          tax_inclusive = log(sum(tax_inclusive)),
                          sales_tax = mean(sales_tax),
                          min_adj_tax = mean(min_adj_tax),
                          lower_adj_tax = mean(lower_adj_tax),
                          tax_diff = mean(tax_diff),
                          tax_ratio = mean(tax_ratio),
                          amazon_collect = mean(amazon_collect)),
                      by = .(year, month, month_counter, fips, state, amazon)]

rm(full_data, transactions)

# ----------------- Regressions (Amazon Transactions) --------------------------
reg1 <- felm(tax_exclusive ~ amazon_collect |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg1, file = paste0(directory, 'reg1.rda'), compress = TRUE)

reg2 <- felm(tax_exclusive ~ log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg2, file = paste0(directory, 'reg2.rda'), compress = TRUE)

reg3 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg3, file = paste0(directory, 'reg3.rda'), compress = TRUE)

reg4 <- felm(tax_inclusive ~ amazon_collect |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg4, file = paste0(directory, 'reg4.rda'), compress = TRUE)

reg5 <- felm(tax_inclusive ~ log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg5, file = paste0(directory, 'reg5.rda'), compress = TRUE)

reg6 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 1])
save(reg6, file = paste0(directory, 'reg6.rda'), compress = TRUE)

# ------------------------- Regressions (non-Amazon Transactions) --------------
reg7 <- felm(tax_exclusive ~ amazon_collect |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 0])
save(reg7, file = paste0(directory, 'reg7.rda'), compress = TRUE)

reg8 <- felm(tax_exclusive ~ log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 0])
save(reg8, file = paste0(directory, 'reg8.rda'), compress = TRUE)

reg9 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
             data = baugh[amazon == 0])
save(reg9, file = paste0(directory, 'reg9.rda'), compress = TRUE)

reg10 <- felm(tax_inclusive ~ amazon_collect |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg10, file = paste0(directory, 'reg10.rda'), compress = TRUE)

reg11 <- felm(tax_inclusive ~ log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg11, file = paste0(directory, 'reg11.rda'), compress = TRUE)

reg12 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg12, file = paste0(directory, 'reg12.rda'), compress = TRUE)

# ---------------- Transactions over $250 -------------------------------
reg13 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg13, file = paste0(directory, 'reg13.rda'), compress = TRUE)

reg14 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg14, file = paste0(directory, 'reg14.rda'), compress = TRUE)

reg15 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg15, file = paste0(directory, 'reg15.rda'), compress = TRUE)

reg16 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg16, file = paste0(directory, 'reg16.rda'), compress = TRUE)

################# All adjacent counties ########################################
# ------------------- Adjacent County Transactions (tax_diff) ------------------
# Amazon only
reg17 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 1])
save(reg17, file = paste0(directory, 'reg17.rda'), compress = TRUE)

reg18 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 1])
save(reg18, file = paste0(directory, 'reg18.rda'), compress = TRUE)

# non-Amazon
reg19 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg19, file = paste0(directory, 'reg19.rda'), compress = TRUE)

reg20 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg20, file = paste0(directory, 'reg20.rda'), compress = TRUE)

# ------------------- Adjacent County Transactions (tax_ratio) ------------------
# Amazon only
reg21 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 1])
save(reg21, file = paste0(directory, 'reg21.rda'), compress = TRUE)

reg22 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 1])
save(reg22, file = paste0(directory, 'reg22.rda'), compress = TRUE)

# non-Amazon
reg23 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg23, file = paste0(directory, 'reg23.rda'), compress = TRUE)

reg24 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[amazon == 0])
save(reg24, file = paste0(directory, 'reg24.rda'), compress = TRUE)

# ---------------- Border county transactions over $250 (tax diff) -------------
# Amazon
reg25 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg25, file = paste0(directory, 'reg25.rda'), compress = TRUE)

reg26 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg26, file = paste0(directory, 'reg26.rda'), compress = TRUE)

# non-Amazon
reg27 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg27, file = paste0(directory, 'reg27.rda'), compress = TRUE)

reg28 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_diff |
               factor(month_counter) + factor(fips) |
               0 |
               fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg28, file = paste0(directory, 'reg28.rda'), compress = TRUE)

# ---------------- Border county transactions over $250 (tax ratio) ------------
# Amazon
reg29 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
                factor(month_counter) + factor(fips) |
                0 |
                fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg29, file = paste0(directory, 'reg29.rda'), compress = TRUE)

reg30 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
                factor(month_counter) + factor(fips) |
                0 |
                fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1])
save(reg30, file = paste0(directory, 'reg30.rda'), compress = TRUE)

# non-Amazon
reg31 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
                factor(month_counter) + factor(fips) |
                0 |
                fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg31, file = paste0(directory, 'reg31.rda'), compress = TRUE)

reg32 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) + tax_ratio |
                factor(month_counter) + factor(fips) |
                0 |
                fips + year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0])
save(reg32, file = paste0(directory, 'reg32.rda'), compress = TRUE)

###################### Only Adjacent Counties with 0 Sales Tax #################
# ------------------- Adjacent County Transactions (tax_diff) ------------------
# Amazon only
reg33 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect) |
              factor(month_counter)   |
                0 |
                 year,
              data = baugh[amazon == 1 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg33, file = paste0(directory, 'reg33.rda'), compress = TRUE)

reg34 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[amazon == 1 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg34, file = paste0(directory, 'reg34.rda'), compress = TRUE)

# non-Amazon
reg35 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[amazon == 0 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg35, file = paste0(directory, 'reg35.rda'), compress = TRUE)

reg36 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[amazon == 0 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg36, file = paste0(directory, 'reg36.rda'), compress = TRUE)

# ---------------- Border county transactions over $250 (tax diff) -------------
# Amazon
reg37 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg37, file = paste0(directory, 'reg37.rda'), compress = TRUE)

reg38 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[tax_exclusive >= log(250) & amazon == 1 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg38, file = paste0(directory, 'reg38.rda'), compress = TRUE)

# non-Amazon
reg39 <- felm(tax_inclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg39, file = paste0(directory, 'reg39.rda'), compress = TRUE)

reg40 <- felm(tax_exclusive ~ log(1 + sales_tax) + log(1 + sales_tax * amazon_collect)   |
                factor(month_counter)   |
                0 |
                 year,
              data = baugh[tax_exclusive >= log(250) & amazon == 0 & min_adj_tax == 0 & lower_adj_tax == 1])
save(reg40, file = paste0(directory, 'reg40.rda'), compress = TRUE)
