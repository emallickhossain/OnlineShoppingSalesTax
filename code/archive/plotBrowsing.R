# ---------------- Plots Browsing Durations and Page Views (per user) ----------
# Loading libraries and data
library(data.table)
library(plotly)
load("/home/mallick/Desktop/comScore/BrowsingDurations.rda")

# Summing page views and creating date variables
finalPlot <- browsing[, .(totalPages = sum(totalPages),
                          totalDuration = sum(totalDuration),
                          totalUsers = length(unique(machine_id))), 
                      keyby = .(year)]

# Merging datasets
finalPlot <- finalPlot[, .(pagesPerUser = totalPages / totalUsers,
                           minutesPerUser = totalDuration / totalUsers), by = .(year)]
plot_ly(finalPlot, x = ~year) %>%
  add_trace(y = ~pagesPerUser, name = "Pages", mode = "lines", 
            type = "scatter") %>%
  add_trace(y = ~minutesPerUser, name = "Minutes", mode = "lines",
            type = "scatter") %>%
  layout(title = "Online Browsing Behavior",
         margin = list(t = 80, l = 80, b = 50),
         font = list(size = 20),
         xaxis = list(title = "Year", range = list(2002, 2015)),
         yaxis = list(title = "Pages or Minutes per User", rangemode = "tozero"),
         showlegend = FALSE)

export(file = "/home/mallick/Dropbox/UPenn/Research/OnlineShopping/Presentation/EmpiricalLunch/perUserBrowsing.png")
