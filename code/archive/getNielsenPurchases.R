# Assembles Nielsen data together
# Only gets categories 0 (Health and beauty), 7 (non-food grocery),
# and 9 (general merchandise). This removes dry grocery, frozen foods, dairy,
# deli, meat, produce, and alcohol
library(data.table)
library(plotly)
retailers <- fread('/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv')

annual_data <- NULL
for (i in 2004:2015) {
  # Loading trip and panelist info
  trips <- fread(paste0('/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/', i,
                        '/Annual_Files/trips_', i, '.tsv'))
  panelists <- fread(paste0('/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/',
                            i, '/Annual_Files/panelists_', i, '.tsv'),
                     select = c('household_code', 'panel_year', 'projection_factor',
                                'household_income', 'race'))

  # Adding household weights and store categories to the trips data
  trips <- merge(trips, panelists, by = c('household_code', 'panel_year'))
  trips <- merge(trips, retailers, by = 'retailer_code')
  annual_household <- trips[, .(total_trips = .N, total_spending = sum(total_spent)),
                            by = .(household_code, retailer_code, channel_type,
                                   projection_factor, panel_year)]
  annual_household <- annual_household[, .(trips = weighted.mean(total_trips,
                                                                 projection_factor),
                                           spending = weighted.mean(total_spending,
                                                                    projection_factor)),
                                       by = .(retailer_code, channel_type, panel_year)]
  annual_data <- rbind(annual_data, annual_household)
  rm(panelists, trips, annual_household)
}

annual_data[, c('total_trips', 'total_spending') := .(sum(trips), sum(spending)),
            by = panel_year]

annual_data[, c('trip_share', 'spending_share') := .(trips / total_trips * 100,
                                                     spending / total_spending * 100)]

# Number of shopping trips
plot_ly(data = annual_data[online == 0], x = ~panel_year,
        y = ~trips, name = 'Offline', type = 'scatter',
        mode = 'lines', height = 800, width = 1200, line = list(width = 10)) %>%
  add_lines(data = annual_data[online == 1], y = ~trips, name = 'Online',
            yaxis = 'y2', line = list(dash = 'dot', width = 10)) %>%
  layout(title = 'Average Shopping Trips by Shopping Mode',
       titlefont = list(size = 35),
       xaxis = list(title = 'Year', titlefont = list(size = 30),
                    tickfont = list(size = 25)),
       yaxis = list(title = 'Number of Trips', range = c(150, 200), dtick = 10,
                    titlefont = list(size = 30), tickfont = list(size = 25)),
       # Pick dtick so that ticks line up on both sides of chart
       yaxis2 = list(title = 'Number of Trips', range = c(0, 10), dtick = 2,
                     side = 'right', overlaying = 'y', tickfont = list(size = 25),
                     titlefont = list(size = 30)),
       # Pick positioning of legend so it doesn't overlap the chart
       legend = list(yanchor = 'top', x = 0.85, font = list(size = 20)),
       # Adjust margins so things look nice
       margin = list(l = 100, r = 90, t = 60, b = 150, pad = 10),
       annotations = list(text = 'Source: Nielsen Consumer Panel. Kilts Marketing Center.',
                          font = list(size = 20),
                          showarrow = FALSE,
                          xref = 'paper', x = -0.03,
                          yref = 'paper', y = -0.22))

# Spending by mode
plot_ly(data = annual_data[online == 0], x = ~panel_year,
        y = ~spending, name = 'Offline', type = 'scatter',
        mode = 'lines', height = 800, width = 1200, line = list(width = 10)) %>%
  add_lines(data = annual_data[online == 1], y = ~spending, name = 'Online',
            yaxis = 'y2', line = list(dash = 'dot', width = 10)) %>%
  layout(title = 'Average Spending by Shopping Mode',
       titlefont = list(size = 35),
       xaxis = list(title = 'Year', titlefont = list(size = 30),
                    tickfont = list(size = 25)),
       yaxis = list(title = 'Dollars', range = c(6800, 7800), dtick = 200,
                    titlefont = list(size = 30), tickfont = list(size = 25)),
       # Pick dtick so that ticks line up on both sides of chart
       yaxis2 = list(title = 'Dollars', range = c(300, 800), dtick = 100,
                     side = 'right', overlaying = 'y', tickfont = list(size = 25),
                     titlefont = list(size = 30)),
       # Pick positioning of legend so it doesn't overlap the chart
       legend = list(yanchor = 'top', x = 0.85, font = list(size = 20)),
       # Adjust margins so things look nice
       margin = list(l = 120, r = 120, t = 60, b = 150, pad = 10),
       annotations = list(text = 'Source: Nielsen Consumer Panel. Kilts Marketing Center.',
                          font = list(size = 20),
                          showarrow = FALSE,
                          xref = 'paper', x = -0.03,
                          yref = 'paper', y = -0.22))

# Offline Spending and trip shares
plot_ly(data = annual_data[online == 0], x = ~panel_year,
        y = ~spending_share, name = 'Spending', type = 'scatter',
        mode = 'lines', height = 800, width = 1200, line = list(width = 10)) %>%
  add_lines(y = ~trip_share, name = 'Trips',
            line = list(dash = 'dot', width = 10)) %>%
  layout(title = 'Offline Share of Total Spending',
       titlefont = list(size = 35),
       xaxis = list(title = 'Year', titlefont = list(size = 30),
                    tickfont = list(size = 25)),
       yaxis = list(title = 'Share', range = c(90, 100), dtick = 2,
                    titlefont = list(size = 30), tickfont = list(size = 25)),
       # Pick positioning of legend so it doesn't overlap the chart
       legend = list(yanchor = 'top', x = 0.85, font = list(size = 20)),
       # Adjust margins so things look nice
       margin = list(l = 120, r = 120, t = 60, b = 150, pad = 10),
       annotations = list(text = 'Source: Nielsen Consumer Panel. Kilts Marketing Center.',
                          font = list(size = 20),
                          showarrow = FALSE,
                          xref = 'paper', x = -0.03,
                          yref = 'paper', y = -0.22))
