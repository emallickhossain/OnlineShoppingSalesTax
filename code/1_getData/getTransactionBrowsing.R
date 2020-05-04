# ------------------ ABOUT ----------------------------------------------------
# This gets all browsing activity on any website that users might purchase
# a product from based on my earlier transaction analysis.

# Loading data and grabbing all websites with transactions that I analyze later
library(data.table)
library(purrr)

# scp /home/mallick/Desktop/Research/OnlineShopping/OnlineShoppingSalesTax/code/0_data/Clean/TransactionsClean.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/NewComScore

transactions <- fread("/home/upenn/hossaine/NewComScore/TransactionsClean.csv")
#load("/home/mallick/Desktop/comScore/Final/TransactionsClean.csv")
domain_ids <- unique(transactions$domain_id)

month <- c(73:215)

# Pulling all browsing history and aggregating duration and pages viewed to the
# machine_id level by month
getTransactionBrowsing <- function(month, sampleSize = -1) {
  print(month)
  res <- dbSendQuery(wrds, paste0("SELECT machine_id, domain_id, duration,
                                  pages_viewed, event_date FROM comscore.ss", month, "m"))
  data <- setDT(dbFetch(res, n = sampleSize))
  dbClearResult(res)

  # Getting only activity transaction domains
  data <- data[domain_id %in% domain_ids]

  # Making sure all numeric columns are actually numeric
  data[, c("machine_id", "pages_viewed", "event_date") :=
             .(as.integer(machine_id), as.integer(pages_viewed),
               as.Date(event_date, format = "%Y-%m-%d"))]

  # Aggregating browsing by machine_id, month, and year
  data[, c("year", "month") := .(year(event_date), month(event_date))]
  browsing <- data[, .(monthlyDuration = sum(duration),
                       monthlyPages = sum(pages_viewed)),
                   by = .(machine_id, year, month, domain_id)]

  # Merging with domain names
  res <- dbSendQuery(wrds, paste0("SELECT domain_id, domain_name from comscore.domains",
                                  year(data$event_date[1])))
  domains <- setDT(dbFetch(res, n = sampleSize))
  dbClearResult(res)
  domains[, c("domain_name") := .(trimws(domain_name))]
  browsing <- merge(browsing, domains, by = "domain_id")
  return(browsing)
}

# Combining transaction browsing data together
full_transaction_browsing <- rbindlist(map(month, getTransactionBrowsing))
fwrite(full_transaction_browsing, "/home/upenn/hossaine/NewComScore/fullTransactionBrowsing.csv")
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/NewComScore/fullTransactionBrowsing.csv /home/mallick/Desktop/Research/OnlineShopping/OnlineShoppingSalesTax/code/0_data/Clean/
