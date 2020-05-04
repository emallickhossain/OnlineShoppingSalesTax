# This records all users and the months in which they were recorded browsing.
library(data.table)
library(purrr)
month <- c(49:60, 73:204)

# Pulling all users each month and recording the week, month, and year they were seen browsing
getPanelistTenure <- function(month, sampleSize = -1) {
  print(month)
  query <- paste0("SELECT machine_id, event_date from comscore.ss", month, "m")
  res <- dbSendQuery(wrds, query)
  data <- setDT(dbFetch(res, n = sampleSize))
  data[, c("week", "year", "event_date") := .(week(event_date), year(event_date), NULL)]
  data <- unique(data)
  data <- data[, lapply(.SD, as.integer)]
  return(data)
}

tenure <- unique(rbindlist(map(month, getPanelistTenure)))
tenure <- tenure[, .(weeks = .N), keyby = machine_id]

# Housekeeping
fwrite(tenure, "/home/upenn/hossaine/NewComScore/PanelistTenure.csv")
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/NewComScore/PanelistTenure.csv /home/mallick/Desktop/Research/OnlineShopping/OnlineShoppingSalesTax/code/0_data/Clean/

# Loading data and getting summary
tenure <- fread("./code/0_data/Clean/PanelistTenure.csv")
quantile(tenure$weeks)
