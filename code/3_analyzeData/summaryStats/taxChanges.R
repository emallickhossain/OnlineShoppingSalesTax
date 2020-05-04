# This looks at tax changes over time
library(data.table)
library(plotly)
library(stringr)
library(maps)
load("/home/mallick/Desktop/comScore/zip_tax_min.rda")
final_zip_tax[, "min_adj_tax" := NULL]
setkey(final_zip_tax, fips, year, month)

# Lagging tax rate
final_zip_tax[, "taxChange" := c(NA, diff(sales_tax)), by = fips]

# Plotting histogram
chart <- plot_ly(data = final_zip_tax[taxChange != 0], x = ~taxChange, type = "histogram",
                 height = 800, width = 1200, nbinsx = 40) %>%
  layout(title = "Histogram of Sales Tax Changes",
         titlefont = list(size = 35),
         xaxis = list(title = "Sales Tax Change", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c(-0.05, 0.05)),
         yaxis = list(title = "Frequency", range = c(0, 15000), dtick = 5000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Tax Data Systems. <br> Note: The above chart excludes months with no changes in tax rates.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.25))
export(chart, "./code/5_figures/taxChangeHist.png")

# Plotting changes by state
final_zip_tax[, "taxChangeInd" := .(taxChange > 0)]
final_zip_tax[, "state" := substr(str_pad(fips, 5, "left", "0"), 1, 2)]
graphData <- final_zip_tax[, .(changes = sum(taxChangeInd, na.rm = TRUE)), by = state]
graphData[, "state" := as.integer(state)]
graphData <- merge(graphData, state.fips, by.x = "state", by.y = "fips")[, .(state, changes, polyname)]
graphData <- unique(graphData, by = "state")
graphData[, "polyname" := gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", polyname, perl=TRUE)]
graphData[, "polyname" := gsub(":.*", "", polyname)]
graphData[polyname == "District Of Columbia", "polyname" := "DC"]

chart <- plot_ly(data = graphData, x = ~polyname, y = ~changes,
                 height = 800, width = 1200, type = "bar") %>%
  layout(title = "Sales Tax Changes by State",
         titlefont = list(size = 35),
         xaxis = list(title = "State", titlefont = list(size = 30),
                      tickfont = list(size = 15)),
         yaxis = list(title = "Frequency", range = c(0, 1500), dtick = 500,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 170, pad = 10),
         annotations = list(text = "Source: Tax Data Systems. <br> Note: An observation is a county-month.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.3))
export(chart, "./code/5_figures/taxChangeByState.png")

# Plotting changes by year
graphData <- final_zip_tax[, .(changes = sum(taxChangeInd, na.rm = TRUE)), by = year]
chart <- plot_ly(data = graphData, x = ~year, y = ~changes,
                 height = 800, width = 1200, type = "scatter", mode = "lines",
                 line = list(width = 5)) %>%
  layout(title = "Sales Tax Changes by Year",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Frequency", range = c(0, 5000), dtick = 1000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Tax Data Systems. <br> Note: An observation is a county month.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.25))
export(chart, "./code/5_figures/taxChangeByYear.png")
