# Running regressions to see tax elasticity
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
full_data <- fread("./code/0_data/Clean/comScoreBrowsingRegData.csv")
full_data <- full_data[monthlyDuration > 0]
bad_domains <- c("google.com", "yahoo.com", "atdmt.com", "msn.com", "aol.com", "go.com",
                 "googlesyndication.com-o02", "microsoft.com", "weatherbug.com",
                 "bankofamerica.com", "netflix.com", "earthlink.com", "chase.com",
                 "wellsfargo.com", "match.com", "usatoday.com", "earthlink.net",
                 "mlb.com", "budget.com", "bluemountain.com", "delta.com", "nationalcar.com",
                 "cheaptickets.com", "capitalone.com", "schwab.com", "taxact.com",
                 "ancestry.com", "ticketsnow.com", "internet-taxprep.com",
                 "netflix.com", "earthlink.net", "dollar.com", "napster.com",
                 "united.com", "hrblock.com", "date.com", "letstalk.com",
                 "bankofamerica.com", "ftd.com", "daysinn.com", "taxactonline.com",
                 "taxbrain.com", "usaa.com", "completetax.com", "discovercard.com",
                 "tickets.com", "equifax.com", "comcast.com", "proflowers.com",
                 "xmradio.com", "1800flowers.com", "jetblue.com", "freetaxusa.com",
                 "perfectmatch.com", "esmarttax.com", "starwoodhotels.com",
                 "linkedin.com", "ronssmokeshop.com", "pensketruckrental.com",
                 "securemingle.com", "cig4u.com", "wingateinns.com", "knightsinn.com",
                 "kycigarettes.com", "chemistry.com", "bigjoesmokeshop.com",
                 "mate1.com", "indiansmokesonline.com", "cigarettessentdirect.com",
                 "cigarettesbyinternet.com", "allofourbutts.com", "silvercloudsmokeshop.com",
                 "areasmoke.com", "cigsales.com", "cheap-cig.com", "livenation.com",
                 "booking.com", "simplysmoke.com", "123smoke.com", "cheap-smokes.biz",
                 "livingsocial.com", "azcigs.com", "groupon.com", "zynga.com",
                 "securecigs.com", "paylessmoke.com", "k2smokes.com", "buycheapcigarettes.com",
                 "cigarettesavers.com", "1stcommerce.net", "upinsmokes.com", "buydiscountcigarettes.com",
                 "mailordercigarettes.biz", "eztobacco.com", "smokinprices.com",
                 "dirtcheapcigs.com", "mycigarettes.com", "pnc.com", "havecigs.com",
                 "tdbank.com", "budgettruck.com", "cigarettesamerica.com", "smokesignals.com",
                 "thompsoncigar.com", "usbank.com", "ticketmaster.ca", "blackpeoplemeet.com",
                 "papajohnsonline.com", "pizzahut.com", "relationshipexchange.com",
                 "ticketweb.com", "fandango.com", "stubhub.com", "taxslayer.com",
                 "eztaxreturn.com", "taxcut.com", "chase.com", "expresstaxrefund.com",
                 "ameritrade.com", "usps.com", "americansingles.com", "wellsfargo.com",
                 "match.com", "ups.com", "regions.com", "movietickets.com", "weatherbug.com")
full_data <- full_data[!domain_name %in% bad_domains]

# ------------------------ Household-Month Time Use ----------------------------
# I collapse the data to household transaction observations. There is an issue
# with California because it adopted its Amazon law in the middle of a month, so
# there are two observations in September 2012 when I collapse this way. Just
# to be safe, I drop these observations.
# Aggregate by year, month, household
total_time <- full_data[state != "CA" & year != 2012 & month != 9,
                        .(log_duration = log(sum(monthlyDuration)),
                          log_pages = log(sum(monthlyPages))),
                        keyby = .(year, month, month_counter,
                                  fips, state,
                                  machine_id, size, age, income, children, race, hispanic,
                                  sales_tax, min_adj_tax,
                                  amazon_collect, tax_diff, amazon,
                                  nontaxed_nonamazon, taxed_nonamazon)]
total_timeB <- full_data[, .(log_duration = log(sum(monthlyDuration)),
                             log_pages = log(sum(monthlyPages))),
                         keyby = .(year, month, month_counter, fips, state,
                                   machine_id, size, age, income, children, race,
                                   hispanic, sales_tax, min_adj_tax,
                                   amazon_collect, tax_diff, amazon,
                                   nontaxed_nonamazon, taxed_nonamazon)]
rm(full_data)

# Formula to iterate over regression specifications
runReg <- function(x, y, data) {
  eq <- formula(paste0(y, " ~ ", x, "| ",
                       "size + age + income + children + race + hispanic + fips + ",
                       "month_counter | 0 | fips"))
  output <- felm(eq, data = data)
  return(output)
}

x <- c("sales_tax",
       "sales_tax + sales_tax : amazon_collect",
       "sales_tax + sales_tax : amazon_collect + tax_diff",
       "sales_tax + sales_tax : amazon_collect + tax_diff + tax_diff : amazon_collect")

# ---------------- Time --------------------------------------------------
y <- "log_duration"

# Amazon Pages
allCounties <- map(x, runReg, data = total_time[amazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = total_time[min_adj_tax == 0 & sales_tax != 0 & amazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Amazon Browsing (Minutes)",
          label = "tab:amazonTime",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Minutes",
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
          out = "./tables/amazon_time_hh.tex")

# Taxed Non-Amazon Transactions
allCounties <- map(x, runReg, data = total_time[taxed_nonamazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = total_time[min_adj_tax == 0 & sales_tax != 0 & taxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Taxed Non-Amazon Browsing (Minutes)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Minutes",
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
          out = "./tables/taxed_nonamazon_time_hh.tex")

# Non-Taxed Non-Amazon Transactions
allCounties <- map(x, runReg, data = total_time[nontaxed_nonamazon == 1], y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = total_time[min_adj_tax == 0 & sales_tax != 0 & nontaxed_nonamazon == 1], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Non-Taxed Non-Amazon Browsing (Minutes)",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Minutes",
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
          out = "./tables/nontaxed_nonamazon_time_hh.tex")

# Total Transactions
allCounties <- map(x, runReg, data = total_time, y = y)
borderCounties <- runReg("sales_tax + sales_tax : amazon_collect",
                         data = total_time[min_adj_tax == 0 & sales_tax != 0], y = y)

stargazer(allCounties, borderCounties,
          no.space = TRUE, omit.stat = "ser", type = "text",
          title = "Total Browsing (Minutes)",
          label = "tab:totalTime",
          column.labels = c("All Counties", "Border Counties"),
          column.separate = c(4, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Log Minutes",
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
          out = "./tables/total_time_hh.tex")


total_time[, "date" := as.Date(paste(year, month, "01", sep = "-"))]
total_timeB[, "date" := as.Date(paste(year, month, "01", sep = "-"))]
reg1 <- felm(log_duration ~ sales_tax | machine_id + date | 0 | state,
             data = total_time)
reg2 <- felm(log_duration ~ sales_tax | machine_id + date | 0 | state,
             data = total_timeB)
stargazer(reg1, reg2, type = "text")
