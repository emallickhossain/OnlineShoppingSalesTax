# This generates sales tax shares of revenues and distributions over time

# -------------------- Getting data from Quandl --------------------------------
library(Quandl)
library(data.table)
library(plotly)
taxes <- c('SALES', 'TOTL', 'INC')
stateTaxes <- expand.grid(state.abb, taxes)
stateTaxes <- paste0(stateTaxes$Var1, stateTaxes$Var2)
states <- paste0('FRED/', stateTaxes, 'TAX')
pop <- paste0('FRED/', state.abb, 'POP')
taxRates <- setDT(Quandl(c(states, pop), start_date = '2000-01-01',
                         end_date = '2016-01-01'))
setnames(taxRates, c('date', stateTaxes, paste0(state.abb, 'POP')))

# ------------------- Reshaping data -------------------------------------------
taxRatesLong <- melt(taxRates, measure = patterns('SALES', 'TOTL', 'INC', 'POP'),
                     variable.name = 'state',
                     value.name = c('sales', 'total', 'income', 'pop'))
setkey(taxRatesLong, date, state)
taxRatesLong <- cbind(taxRatesLong,
                      stateName = rep(state.abb, uniqueN(taxRatesLong$date)))

# ------------------- Getting share of state revenues --------------------------
taxRatesLong[, c('salesShare', 'incomeShare') :=
               .(sales / total * 100, income / total * 100)]
aggregateRates <- taxRatesLong[, .(salesShare = weighted.mean(salesShare, w = pop, na.rm = TRUE),
                                   incomeShare = weighted.mean(incomeShare, w = pop, na.rm = TRUE)),
                               by = year(date)]

# -------------------- Plot aggregate share over time --------------------------
plot_ly(data = aggregateRates, x = ~year, y = ~salesShare,
        type = 'scatter', mode = 'lines', line = list(width = 10),
        width = 1200, height = 800) %>%
  add_lines(y = ~incomeShare, line = list(width = 10)) %>%
  layout(title = 'General Sales Tax Share of <br> State Tax Revenues (2000-2015)',
       titlefont = list(size = 35),
       xaxis = list(title = 'Year', titlefont = list(size = 25),
                    tickfont = list(size = 20)),
       yaxis = list(title = 'Percent', range = c(32, 45),
                    titlefont = list(size = 25), tickfont = list(size = 20)),
       # Adjust margins so things look nice
       margin = list(l = 80, r = 50, t = 60, b = 70, pad = 10),
       annotations = list(text = 'Source: U.S. Census Bureau.',
                          font = list(size = 15),
                          showarrow = FALSE,
                          xref = 'paper', x = -0.03,
                          yref = 'paper', y = -0.12))
export(file = '/home/mallick/Dropbox/Research/OnlineShopping/paper/charts/salesTaxShare.png')

# ------------------- Plot estimated density by year ---------------------------
taxDensity <- taxRatesLong[, .(x = density(salesShare, na.rm = TRUE)$x,
                               y = density(salesShare, na.rm = TRUE)$y),
                           by = year(date)]
plot_ly(data = taxDensity, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
        color = ~as.character(year), colors = 'Blues', line = list(width = 10)) %>%
  layout(title = 'Shares of State Revenues from General <br> Sales Taxes Distribution (2000-2015)',
         titlefont = list(size = 35),
         xaxis = list(title = 'Percent of State Revenue',
                      titlefont = list(size = 30),
                      range = c(0, 80), dtick = 10,
                      tickfont = list(size = 25)),
         yaxis = list(title = 'Density', range = c(0, 0.06),
                      dtick = 0.01, titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 50, t = 60, b = 70, pad = 10),
         annotations = list(text = 'Source: U.S. Census Bureau.',
                            font = list(size = 20),
                            showarrow = FALSE,
                            xref = 'paper', x = -0.03,
                            yref = 'paper', y = -0.12))
export(file = '/home/mallick/Dropbox/Research/OnlineShopping/paper/charts/salesTaxShareDistribution.png')
