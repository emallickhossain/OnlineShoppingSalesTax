# ---------------- ABOUT -------------------------------------------------------
# This gets browsing durations and page views by domain_name for each month

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
for (j in 1:length(months)) {
  browsing <- NULL
  loop <- months[[j]]
  for (i in loop) {
    print(i)

    # 2002 is a special case
    if (i %in% 31:36) {
      res <- dbSendQuery(wrds, paste0('select domain_id, duration, page_viewed,
                                       date_id from COMSCORE.SS', i, 'M'))
      data <- setDT(fetch(res, n = sampleSize))
      setnames(data, c('date_id', 'page_viewed'), c('event_date', 'pages_viewed'))
    } else {
      res <- dbSendQuery(wrds, paste0('select domain_id, duration, pages_viewed,
                                      event_date from COMSCORE.SS', i, 'M'))
      data <- setDT(fetch(res, n = sampleSize))
    }

    # Making sure all numeric columns are actually numeric and converting date to Date format
    data[, c('domain_id', 'duration', 'pages_viewed', 'event_date') :=
           .(as.numeric(domain_id), as.numeric(duration), as.numeric(duration),
             as.Date(event_date, format = '%Y-%m-%d'))]

    # Grabbing domains
    res <- dbSendQuery(wrds, paste0('select domain_id, domain_name from
                                    COMSCORE.DOMAINS', year(data$event_date[1])))
    domains <- setDT(fetch(res, n = sampleSize))
    domains[, c('domain_id') := .(as.numeric(domain_id))]

    # Merging domains and browsing
    fullData <- merge(domains, data, by = 'domain_id')
    fullData[, c('domain_name') := .(trimws(domain_name))]
    totals <- fullData[, .(totalDuration = sum(duration),
                           totalPages = sum(pages_viewed),
                           year = mean(year(event_date)),
                           month = mean(month(event_date))),
                       by = domain_name]
    browsing <- rbind(browsing, totals)
    rm(data, res, totals)
  }
  save(browsing, file = paste0('/home/upenn/hossaine/NewComScore/Data/DomainBrowsing',
                               browsing$year[1],'.rda'), compress = TRUE)
}
