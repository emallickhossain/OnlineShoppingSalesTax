# Generates the population map of the comScore panelists and actual populations
# ------------------------ comScore data map -----------------------------------
load('/home/mallick/Desktop/comScore/DemographicsClean.rda')
zip <- fread('/home/mallick/Desktop/comScore/zipState2014.csv')
setnames(zip, "zcta5", "zip_code")
fullMap <- merge(demographics, zip, by = c('year', 'zip_code'))
fullMap <- fullMap[, .N, by = state]
chart <- plot_ly(data = tableData, locationmode = 'USA-states', type = 'choropleth') %>%
  add_trace(z = ~data, locations = ~as.character(state), showscale = FALSE) %>%
  colorbar(title = 'Population') %>%
  layout(title = 'comScore Population',
         geo = list(scope = 'usa', projection = list(type = 'albers usa')),
         font = list(size = 35),
         margin = list(l = 10, r = 10, t = 50, b = 10),
         paper_bgcolor = toRGB(NA, 0),
         plot_bgcolor = toRGB(NA, 0))
export(chart, file = '../5_figures/comScoreGeography.png')

# ----------------------- Census map -------------------------------------------
states <- paste0('FRED/', state.abb, 'POP')
population <- setDT(Quandl(states, start_date = '2016-01-01'))
setnames(population, c('date', state.abb))
population <- cbind(state.abb, transpose(population)[2:ncol(population)])
chart <- plot_geo(data = population, locationmode = 'USA-states') %>%
  add_trace(z = ~V1, locations = ~state.abb, showscale = FALSE) %>%
  colorbar(title = 'Population') %>%
  layout(title = 'Census Population (2016)',
         geo = list(scope = 'usa', projection = list(type = 'albers usa')),
         font = list(size = 35),
         margin = list(l = 10, r = 10, t = 50, b = 10),
         paper_bgcolor = toRGB(NA, 0),
         plot_bgcolor = toRGB(NA, 0))

export(chart, file = '../5_figures/censusGeography.png')
