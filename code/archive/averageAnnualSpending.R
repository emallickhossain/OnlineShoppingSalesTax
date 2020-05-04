library(plotly)
library(data.table)

# Getting average annual spending
load('/home/mallick/Desktop/comScore/TransactionsClean.rda')
graphData <- transactions[, .(totalSpending = sum(prod_totprice)),
                          by = .(machine_id, year)]
graphData <- graphData[, .(avgSpending = mean(totalSpending)), by = year]

# Plotting average spending
plot_ly(data = graphData, x = ~year, y = ~avgSpending, type = 'scatter',
        mode = 'lines', height = 800, width = 1200, line = list(width = 5)) %>%
  layout(title = 'Average Annual Spending',
       titlefont = list(size = 35),
       xaxis = list(title = 'Year', titlefont = list(size = 30),
                    tickfont = list(size = 25)),
       yaxis = list(title = 'Average Annual Spending', range = c(250, 500), dtick = 50,
                    titlefont = list(size = 30), tickfont = list(size = 25)),
       # Adjust margins so things look nice
       margin = list(l = 100, r = 50, t = 60, b = 150, pad = 10),
       annotations = list(text = 'Source: comScore.',
                          font = list(size = 20),
                          showarrow = FALSE,
                          xref = 'paper', x = -0.03,
                          yref = 'paper', y = -0.2))

export(file = '/home/mallick/Dropbox/Research/OnlineShopping/code/5_figures/averageSpending.png')
