# Running regressions to see tax elasticity
library(data.table)
library(plotly)
load("/home/mallick/Desktop/Nielsen/Data/regressionDataNonGrocery.rda")

# Chart of Average Monthly Spending
monthlySpending <- householdSpending[, .(monthlySpending = weighted.mean(totalSpending, w = projection_factor),
                                         onlineSpending = weighted.mean(onlineSpending, w = projection_factor)),
                                     by = .(panel_year, month)]
monthlySpending[, "date" := as.Date(paste(panel_year, month, "01", sep = "-"))]
plot_ly(data = monthlySpending, x = ~date, y = ~monthlySpending, type = "scatter", mode = "lines")

# Histogram of household spending by type
chart <- plot_ly(data = householdSpending[onlineSpending > 1], x = ~onlineSpending, type = "histogram",
                 name = "Total Spending", height = 800, width = 1200, name = "Online Spending",
                 nbinsx = 400) %>%
  #add_histogram(x = ~totalSpending, name = "Total Spending") %>%
  layout(title = "Density of Nielsen Spending",
         barmode = "overlay",
         titlefont = list(size = 35),
         xaxis = list(title = "Spending", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c(0, 500)),
         yaxis = list(title = "Density", range = c(0, 150000), dtick = 50000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 100, pad = 10),
         annotations = list(text = "Source: Nielsen",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.15))
export(chart, file = "./code/5_figures/nielsenHHSpendingPDF.png")
