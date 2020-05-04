# This calculates the following:
# 1. Percentiles of minutes and page views of browsing by each domain
# 2. The number of sites that record at least one transactions
# 3. Constructs the top 10 websites by browsing and page views for each year
library(data.table)
library(plotly)
years <- c(2002, 2004, 2006:2016)

#----------- Getting top 10 websites by duration and page views by year -------
top10Duration <- NULL
top10Pages <- NULL

for (i in years) {
  print(i)
  load(paste0("/home/mallick/Desktop/comScore/DomainBrowsing/DomainBrowsing", i, ".rda"))
  browsing <- browsing[, .(totalDuration = sum(totalDuration),
                           totalPages = sum(totalPages),
                           year = mean(year)),
                       by = domain_name]
  browsing[, c('duration_share', 'page_share', 'nDomains') :=
             .(totalDuration / sum(browsing$totalDuration) * 100,
               totalPages / sum(totalPages) * 100,
               length(unique(domain_name)))]
  setkey(browsing, duration_share)
  top10Duration <- rbind(top10Duration, browsing[, tail(.SD, 10)])
  setkey(browsing, page_share)
  top10Pages <- rbind(top10Pages, browsing[, tail(.SD, 10)])
  rm(browsing)
}

top10Duration <- top10Duration[, .(duration_share = sum(duration_share)), keyby = year]
top10Pages <- top10Pages[, .(page_share = sum(page_share)), keyby = year]

plot_ly(data = top10Duration, x = ~years, y = ~duration_share,
        name = 'Duration', type = 'scatter', mode = 'lines') %>%
  add_trace(data = top10Pages, y = ~page_share, name = 'Pages', type = 'scatter', mode = 'lines') %>%
  layout(yaxis = list(title = 'Percent', rangemode = 'tozero'),
         xaxis = list(title = 'Year'),
         title = "Top 10 Website Shares of Total Browsing")
export(file = "/home/mallick/Dropbox/UPenn/Research/OnlineShopping/paper/charts/top10BrowsingShares.png")

#------- Getting percentiles of browsing duration and page views by year and unique domains
plot_ly(data = data.table(), x = ~years) %>%
  add_trace(y = ~nDomains, type = 'scatter', mode = 'lines') %>%
  layout(yaxis = list(title = 'Number of Unique Domains', rangemode = 'tozero'),
         xaxis = list(title = 'Year'),
         title = 'Number of Unique Domains Visited by Year')
export(file = "/home/mallick/Dropbox/Research/OnlineShopping/paper/charts/domainsVisited.png")

#-- Getting total unique shopping sites (with at least 1 transaction) by year --
load("/home/mallick/Desktop/comScore/Transactions.rda")
sales <- transactions[, .(unique_domains = length(unique(domain_name)),
                          total_transactions = length(unique(site_session_id))), keyby = year]

plot_ly(data = sales, x = ~year) %>%
  add_trace(y = ~unique_domains, type = 'scatter', mode = 'lines', name = 'Domains') %>%
  add_trace(y = ~total_transactions, type = 'scatter', mode = 'lines', name = 'Transactions', yaxis = 'y2') %>%
  layout(title = 'Unique Domains with Recorded Transactions and Transaction Volumes (2002-2016)',
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Number of Domains', range = c(0, 700)),
         yaxis2 = list(title = 'Number of Transactions', side = 'right', overlaying = 'y', range = c(0, 700000)))
export(file = "/home/mallick/Dropbox/Research/OnlineShopping/paper/charts/uniqueTransactionDomains.png")
