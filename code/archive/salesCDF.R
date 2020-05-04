# Total Sales per website over 2004, 2006-2016
library(data.table)
library(plotly)
load('/home/mallick//Desktop/comScore/TransactionsClean.rda')

# Computing sales shares by website
sales_by_site <- transactions[, .(total_sales = sum(prod_totprice)), by = .(domain_name, year)]
sales_by_site[, 'annual_sales' := sum(total_sales), by = year]
sales_by_site[, 'site_share' := total_sales / annual_sales * 100]

# Ranking and generating CDF
sales_by_site[, 'rank' := frankv(site_share, order = -1), by = year]
setorder(sales_by_site, year, -site_share)
sales_by_site[, 'CDF' := cumsum(site_share), by = year]

# Plotting
chart <- plot_ly(data = sales_by_site, x = ~rank, y = ~CDF, type = 'scatter',
                 mode = 'lines', color = ~as.character(year),
                 colors = 'Blues', height = 800, width = 1200,
                 line = list(width = 10)) %>%
  layout(title = 'CDF of Total Sales by Sales Rank (2004-2016)',
         titlefont = list(size = 35),
         xaxis = list(title = 'Sales Rank', titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c(0, 250)),
         yaxis = list(title = 'Share', range = c(0, 100), dtick = 20,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn't overlap the chart
         legend = list(font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = 'Source: comScore Web Behavior Database.',
                            font = list(size = 20),
                            showarrow = FALSE,
                            xref = 'paper', x = -0.03,
                            yref = 'paper', y = -0.22))

export(chart, file = '/home/mallick/Dropbox/Research/OnlineShopping/code/5_figures/rankCDF.png')
#export(file = '../../5_figures/rankCDF.png')
