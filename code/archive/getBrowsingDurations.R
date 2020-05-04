# ---------------- ABOUT -------------------------------------------------------
# This gets all browsing data from comScore and aggregates it together to get
# browsing durations and page views by month for each machine_id.

# ---------------- CODE --------------------------------------------------------
# Import libraries
library(data.table)

# Vector of years to loop over
years <- list(y2002 = 31:36, y2004 = 49:60, y2006 = 73:84, y2007 = 85:96,
              y2008 = 97:108, y2009 = 109:120, y2010 = 121:132, y2011 = 133:144,
              y2012 = 145:156, y2013 = 157:168, y2014 = 169:180, y2015 = 181:192,
              y2016 = 193:204)
sampleSize <- -1
browsing <- NULL

# Pulling all browsing history and aggregating duration and pages viewed to the
# machine_id level by month
for (j in 1:length(years)) {
  loop <- years[[j]]
  for (i in loop) {
    print(i)
    if (i %in% 31:36) {
      res <- dbSendQuery(wrds, paste0("select machine_id, duration,
                                       page_viewed, date_id from
                                       COMSCORE.SS", i, "M"))
      data <- setDT(fetch(res, n = sampleSize))
      setnames(data, "date_id", "event_date")
      setnames(data, "page_viewed", "pages_viewed")
    } else {
      res <- dbSendQuery(wrds, paste0("select machine_id, duration,
                                       pages_viewed, event_date from
                                       COMSCORE.SS", i, "M"))
      data <- setDT(fetch(res, n = sampleSize))
    }
    data$event_date <- as.Date(data$event_date, format = "%Y-%m-%d")
    totals <- data[, .(totalDuration = sum(duration),
                       totalPages = sum(pages_viewed),
                       year = mean(year(event_date)),
                       month = mean(month(event_date))),
                   by = machine_id]
    browsing <- rbind(browsing, totals)
    rm(data, res, totals)
  }
}
save(browsing, file = paste0("/home/upenn/hossaine/NewComScore/Data/BrowsingDurations.rda"),
     compress = TRUE)
