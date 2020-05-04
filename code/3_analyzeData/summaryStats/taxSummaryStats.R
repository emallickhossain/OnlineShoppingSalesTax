library(data.table)
library(plotly)
load("/home/mallick/Desktop/comScore/comScoreTransactionReg.rda")

# Tax Differences
meanTaxDiff <- full_data[tax_diff >= 0.001 & !is.na(tax_diff),
                         .(taxDiff = mean(tax_diff, na.rm = TRUE) * 100), by = machine_id]
chart <- plot_ly(data = meanTaxDiff, x = ~taxDiff, type = "histogram",
                 height = 800, width = 1200, nbinsx = 40) %>%
  layout(title = "Histogram of Sales Tax Differences",
         titlefont = list(size = 35),
         xaxis = list(title = "Sales Tax Difference", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c(0, 10)),
         yaxis = list(title = "Frequency", range = c(0, 50000), dtick = 10000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 130, pad = 10),
         annotations = list(text = "Source: Tax Data Systems.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/taxDiffHist.png")

# Tax Rates
meanTaxRate <- full_data[, .(sales_tax = mean(sales_tax, na.rm = TRUE) * 100), by = machine_id]
chart <- plot_ly(data = meanTaxRate, height = 800, width = 1200) %>%
  add_histogram(x = ~sales_tax, nbinsx = 40) %>%
  layout(title = "Histogram of Sales Tax",
         titlefont = list(size = 35),
         xaxis = list(title = "Sales Tax", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Frequency", range = c(0, 55000), dtick = 10000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 130, pad = 10),
         annotations = list(text = "Source: Tax Data Systems.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/taxRateHist.png")
