# This plots the number of states that Amazon collects sales tax in over time
library(data.table)
library(plotly)
library(readxl)

# Loading data
collection <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))

# Getting Census State populations
pop <- na.omit(fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/national/totals/nst-est2017-alldata.csv",
                     select = c("NAME", "POPESTIMATE2017")))
pop[, "totalPop" := pop[NAME == "United States"]$POPESTIMATE2017]
pop[, "share" := POPESTIMATE2017 / totalPop * 100]
pop <- pop[, .(NAME, share)]
collection <- merge(collection, pop, by.x = "State", by.y = "NAME")

# Counting number of states collecting sales tax by date
collection <- collection[, .(AmazonCollected = as.Date(AmazonCollected),
                             states = 0,
                             popShare = share,
                             State = State)]
setkey(collection, AmazonCollected)
collection[!is.na(AmazonCollected), "states" := 1]
collection[, ':=' (states = cumsum(states),
                   pop = cumsum(popShare))]
collection <- collection[, .(states = max(states),
                             pop = max(pop)), by = AmazonCollected]

# Plotting
chart <- plot_ly(data = collection, x = ~AmazonCollected, y = ~states,
                 type = "scatter", mode = "lines", height = 800, width = 1200,
                 line = list(width = 5)) %>%
  layout(title = "Amazon Sales Tax Collection",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c("2008-01-01", "2017-12-31")),
         yaxis = list(title = "Number of States", range = c(0, 50), dtick = 10,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 140, pad = 10),
         annotations = list(text = "Source: Author's calculations based on news reports.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.22))
export(chart, file = "./code/5_figures/amazonCollection.png")

# Population plot
chart <- plot_ly(data = collection, x = ~AmazonCollected, y = ~pop,
                 type = "scatter", mode = "lines", height = 800, width = 1200,
                 line = list(width = 5)) %>%
  layout(title = "Amazon Sales Tax Collection",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c("2008-01-01", "2017-12-31")),
         yaxis = list(title = "Share of US Population", range = c(0, 100), dtick = 20,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 140, pad = 10),
         annotations = list(text = "Source: Author's calculations based on news reports.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.22))
export(chart, file = "./code/5_figures/amazonCollectionPop.png")
