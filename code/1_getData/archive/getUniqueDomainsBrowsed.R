# Gets count of unique domains browsed by month
library(data.table)
library(purrr)
library(furrr)
months <- c(49:60, 73:204)

# Pulling all unique domains by month
getUniqueDomains <- function(month, sampleSize = -1) {
  print(month)
  query <- paste0("SELECT DISTINCT domain_id, event_date FROM comscore.ss", month, "m")
  res <- dbSendQuery(wrds, query)
  data <- unique(setDT(dbFetch(res, n = sampleSize)))
  dbClearResult(res)
  return(data)
}

browsing <- rbindlist(future_map(months, getUniqueDomains))
browsing[, "year" := substr(event_date, 1, 4)]
annualCount <- browsing[, .(count = uniqueN(domain_id)), by = year]
fwrite(annualCount, file = "/home/mallick/Desktop/comScore/uniqueDomainsBrowsed.csv")
