# ---------------- ABOUT -------------------------------------------------------
# This gets total number of machines browsing by month

# ---------------- CODE --------------------------------------------------------
# Import libraries
library(data.table)

# Vector of years to loop over
months <- list(y2002 = 31:36, y2004 = 49:60, y2006 = 73:84, y2007 = 85:96,
               y2008 = 97:108, y2009 = 109:120, y2010 = 121:132, y2011 = 133:144,
               y2012 = 145:156, y2013 = 157:168, y2014 = 169:180, y2015 = 181:192,
               y2016 = 193:204)
sampleSize <- -1

# Pulling all browsing history and aggregating duration and pages viewed to the
# machine_id level by month
users <- NULL
for (j in 1:12) {
  loop <- months[[j]]
  for (i in loop) {
    print(i)
    if (i %in% 31:36) {
      res <- dbSendQuery(wrds, paste0("select machine_id, date_id from
                                      COMSCORE.SS", i, "M"))
      data <- setDT(fetch(res, n = sampleSize))
      setnames(data, "date_id", "event_date")
    } else {
      res <- dbSendQuery(wrds, paste0("select machine_id, event_date from
                                      COMSCORE.SS", i, "M"))
      data <- setDT(fetch(res, n = sampleSize))
    }
    data[, 'event_date' := as.Date(event_date, format = "%Y-%m-%d")]
    data[, c("year", "month") := .(year(event_date), month(event_date))]
    total <- data[, .(totalUsers = uniqueN(machine_id)), by = .(year, month)]
    users <- rbind(users, total)
    rm(data, res)
  }
}
save(users, file = paste0("/home/upenn/hossaine/NewComScore/Data/BrowsingUsers.rda"),
     compress = TRUE)
