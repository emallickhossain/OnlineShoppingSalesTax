# ------------------ ABOUT ----------------------------------------------------
# This gets the number of browsing sessions a household has each week

# Loading data and grabbing all websites with transactions that I analyze later
library(data.table)
library(purrr)

month <- c(49:60, 73:204)

# Pulling all browsing history and aggregating duration and pages viewed to the
# machine_id level by month
getBrowsingSessions <- function(month, sampleSize = -1) {
  print(month)
  res <- dbSendQuery(wrds, paste0("SELECT machine_id, site_session_id,
                                  event_date FROM comscore.ss", month, "m"))
  data <- setDT(dbFetch(res, n = sampleSize))
  dbClearResult(res)

  # Making sure all numeric columns are actually numeric
  data[, c("machine_id", "event_date") :=
         .(as.integer(machine_id), as.Date(event_date, format = "%Y-%m-%d"))]

  # Aggregating Amazon browsing by machine_id, month, and year
  data[, c("year", "month", "week") :=
         .(as.integer(year(event_date)), as.integer(month(event_date)),
           as.integer(week(event_date)))]
  sessions <- data[, .(nSessions = as.integer(uniqueN(site_session_id))),
                   by = .(machine_id, year, month, week)]
  return(sessions)
}

# Combining transaction browsing data together
browsing_sessions <- rbindlist(map(month, getBrowsingSessions))
fwrite(browsing_sessions, file = "/home/upenn/hossaine/NewComScore/Data/browsingSessions.csv")
