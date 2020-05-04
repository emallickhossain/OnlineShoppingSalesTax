# ---------------- ABOUT -------------------------------------------------------
# This gets all transactions from comScore and cleans it

# ---------------- CODE --------------------------------------------------------
# Import libraries
library(data.table)
library(purrr)
library(fredr)
library(readxl)
library(stargazer)
fredr_set_key(fredAPI)
years <- c(2006:2016)

# Pulling and appending all domain names to the transaction data
getTrans <- function(year, sampleSize = -1) {
  transTable <- paste0("comscore.ptrans", year)
  domainTable <- paste0("comscore.domains", year)

  # Doing left join in SQL to keep all transactions even if domain is not recorded
  query <- paste0("SELECT ", domainTable, ".domain_name, ", domainTable, ".domain_id,",
                  "site_session_id, machine_id, prod_category_id, ",
                  "prod_totprice, basket_tot, event_date ",
                  "FROM ", transTable,
                  " LEFT JOIN ", domainTable,
                  " ON ", domainTable, ".domain_id = ", transTable, ".domain_id")
  res <- dbSendQuery(wrds, query)
  data <- na.omit(setDT(dbFetch(res, n = sampleSize)))
  dbClearResult(res)
  return(data)
}

transactions <- rbindlist(map(years, getTrans), use.names = TRUE)
fwrite(transactions, "./code/0_data/rawTransactions.csv")

# ------------------------- CLEANING -------------------------------------------
# Removing duplicate entries
row1 <- c("Starting Transactions:", nrow(transactions))

transactions <- unique(transactions)
row2 <- c("Unduplicated Transactions:", nrow(transactions))

# Removing non-Amazon categories
transactions <- transactions[prod_category_id %in% c(1:12, 14:17, 19:40, 54:56, 99)]
row3 <- c("Amazon Categories:", nrow(transactions))

# Removing invalid prices and non-Amazon categories
transactions <- transactions[!is.na(prod_totprice) &
                             prod_totprice >= 1.0 & basket_tot >= 1.0 &
                             prod_totprice <= 500]

# I remove the site sessions with transactions in which the basket total is
# higher than 10% of the products plus $100. I also remove any transactions where
# the basket price is more than 75% off the product price
price_bugs <- transactions[, .(products = sum(prod_totprice),
                               basket = mean(basket_tot)),
                           by = .(site_session_id, domain_name)]
price_bugs[, "difference" := basket - products]
ok_basket <- price_bugs[difference < (1.1 * products + 100) &
                        basket > (0.25 * products)]$site_session_id
transactions <- transactions[site_session_id %in% ok_basket]
row4 <- c("Invalid Prices:", nrow(transactions))

# Removing misclassified domains (at least for Amazon competitive categories)
search <- c("google.com", "yahoo.com", "yahoo.net", "msn.com", "yahoo.co.jp",
            "ussearch.com")
isp_phone <- c("att.com", "aol.com", "bellsouth.com", "earthlink.net",
               "comcast.com", "cingular.com", "cox.com", "metropcs.com",
               "mycricket.com", "qwestwireless.com", "sprint.com", "t-mobile.com",
               "verizon.com", "cricketwireless.com", "greendot.com", "sprintpcs.com",
               "tracfone-orders.com", "verizonwireless.com", "boostmobile.com",
               "motorola.com", "tracfone.com", "boostmobilestore.com", "nextel.com",
               "virginmobileusa.com", "charter.com", "apple.com")
travel <- c("budget.com", "delta.com", "nationalcar.com", "cheaptickets.com",
            "dollar.com", "united.com", "daysinn.com", "jetblue.com",
            "starwoodhotels.com", "pensketruckrental.com", "wingateinns.com",
            "knightsinn.com", "budgettruck.com", "usairways.com", "uhaul.com",
            "booking.com", "exitravel.com")
finance <- c("capitalone.com", "schwab.com", "taxact.com", "hrblock.com",
             "taxactonline.com", "taxbrain.com", "usaa.com", "completetax.com",
             "discovercard.com", "internet-taxprep.com", "bankofamerica.com",
             "equifax.com", "freetaxusa.com", "esmarttax.com", "pnc.com",
             "tdbank.com", "usbank.com", "taxslayer.com", "eztaxreturn.com",
             "taxcut.com", "chase.com", "expresstaxrefund.com", "ameritrade.com",
             "wellsfargo.com", "regions.com", "ally.com", "bbt.com", "citibank.com",
             "americanexpress.com", "myfico.com", "1stcommerce.net",
             "walmartmoneycard.com", "intuit.com", "checksunlimited.com",
             "styleschecks.com", "mypoints.com")
tickets <- c("ticketsnow.com", "tickets.com", "livenation.com", "ticketmaster.ca",
             "ticketweb.com", "fandango.com", "stubhub.com", "movietickets.com",
             "ticketliquidator.com", "ticketexchangebyticketmaster.com",
             "ticketmaster.com", "movielink.com", "mymusic.com")
cigs <- c("ronssmokeshop.com", "cig4u.com", "kycigarettes.com", "bigjoesmokeshop.com",
          "indiansmokesonline.com", "cigarettessentdirect.com",
          "cigarettesbyinternet.com", "allofourbutts.com", "silvercloudsmokeshop.com",
          "areasmoke.com", "cigsales.com", "cheap-cig.com", "simplysmoke.com",
          "123smoke.com", "cheap-smokes.biz", "azcigs.com", "securecigs.com",
          "paylessmoke.com", "k2smokes.com", "buycheapcigarettes.com",
          "cigarettesavers.com", "upinsmokes.com", "buydiscountcigarettes.com",
          "mailordercigarettes.biz", "eztobacco.com", "smokinprices.com",
          "dirtcheapcigs.com", "mycigarettes.com", "havecigs.com",
          "cigarettesamerica.com", "smokesignals.com", "thompsoncigar.com")
dating <- c("date.com", "letstalk.com", "perfectmatch.com", "securemingle.com",
            "chemistry.com", "mate1.com", "blackpeoplemeet.com",
            "relationshipexchange.com", "americansingles.com", "match.com")
entertainment <- c("netflix.com", "napster.com", "xmradio.com", "zynga.com",
                   "audible.com", "nin.com", "nintendo.com", "playboy.com",
                   "wwe.com", "mlb.com", "ign.com", "go.com", "nascar.com",
                   "playboystore.com", "wweshop.com", "mlbshop.com", "nflshop.com",
                   "gamehouse.com", "rivals.com", "ea.com")
news <- c("britannica.com", "usatoday.com", "washpost.com", "usnews.com")
food <- c("papajohnsonline.com", "pizzahut.com", "papajohns.com", "peapod.com",
          "omahasteaks.com", "winecountrygiftbaskets.com", "nutrisystem.com")
other <- c("bluemountain.com", "ancestry.com", "ftd.com", "proflowers.com",
           "1800flowers.com", "linkedin.com", "livingsocial.com", "groupon.com",
           "usps.com",  "ups.com", "weatherbug.com", "adobe.com",
           "costcophotocenter.com", "shockwave.com", "shutterfly.com",
           "vistaprint.com", "cvsphoto.com", "kodakgallery.com", "autotrader.com",
           "jacksonandperkins.com", "amway.com", "avon.com", "canadiantire.ca",
           "myherbalife.com", "quixtar.com", "tirerack.com", "windowsonecare.com",
           "melaleuca.com", "weightwatchers.com", "mannatech.com", "mcafee.com",
           "snapfish.com")
bad_domains <- c(search, isp_phone, travel, finance, tickets, cigs, dating,
                 entertainment, news, food, other)
transactions <- transactions[!domain_name %in% bad_domains]
row5 <- c("Invalid Domains:", nrow(transactions))

# Final count after merging
cleanTable <- as.data.table(rbind(row1, row2, row3, row4, row5))
setnames(cleanTable, c("Step", "Transactions"))
cleanTable[, "Transactions" := as.integer(Transactions)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "comScore Transactions",
          label = "tab:transactionsClean", digits = 2, rownames = FALSE,
          out = "./tables/transactionsClean.tex")

# Deflate transaction amounts to December 2016 dollars to get real transactions
transactions[, c("year", "month") := .(as.integer(substr(event_date, 1, 4)),
                                       as.integer(substr(event_date, 6, 7)))]
cpi <- setDT(fredr("CPIAUCSL", observation_start = as.Date("2004-01-01"),
                   observation_end = as.Date("2016-12-31")))[, "series_id" := NULL ]
cpi[, c("year", "month", "date") := .(as.integer(year(date)), as.integer(month(date)), NULL)]
setnames(cpi, "value", "cpi")
transactions <- merge(transactions, cpi, by = c("year", "month"))
dec2016 <- cpi[year == 2016 & month == 12]$cpi
transactions[, c("prod_totprice_real", "basket_tot_real") :=
               .(prod_totprice / cpi * dec2016, basket_tot / cpi * dec2016)]

# Adding online/offline indicator
online_offline <- setDT(read_excel("./code/0_data/onlineOffline.xls"))
online_offline <- online_offline[, .(domain_name, Offline)]
transactions <- merge(transactions, online_offline, by = "domain_name", all.x = TRUE)
transactions[is.na(Offline), "Offline" := "No"]

# Saving cleaned transaction data
transactions[, c("site_session_id", "basket_tot", "basket_tot_real", "cpi") := NULL]
fwrite(transactions, file = "./code/0_data/Clean/TransactionsClean.csv")
uniqueN(transactions$machine_id)
