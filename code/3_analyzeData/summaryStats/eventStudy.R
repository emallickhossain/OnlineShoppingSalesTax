# Making event study for sales tax collection
library(data.table)
library(readxl)
library(plotly)

# Load transaction and demographic data
load("/home/mallick/Desktop/comScore/DemographicsClean.rda")
load("/home/mallick/Desktop/comScore/TransactionsClean.rda")

# Keeping necessary info
demographics <- demographics[, .(machine_id, year, state)]
transactions <- transactions[, .(year, month, machine_id, domain_name, prod_totprice_real)]
full_data <- merge(demographics, transactions, by = c("year", "machine_id"))
rm(transactions, demographics)

# Merging in Amazon tax sheet
amazon_tax <- setDT(read_excel("./code/0_data/AmazonLaws.xls"))
amazon_tax <- amazon_tax[, .(state, AmazonCollected)]
full_data <- merge(full_data, amazon_tax, by = "state")

# Getting online offline status
offline <- setDT(read_excel("./code/0_data/onlineOffline.xls"))
offline[, c("totalSales", "Notes") := NULL]
full_data <- merge(full_data, offline, by = "domain_name")

# Generating categorical variable
full_data[domain_name == "amazon.com", "type" := "Amazon"]
full_data[domain_name != "amazon.com" & Offline == "Yes", "type" := "Taxed Non-Amazon"]
full_data[domain_name != "amazon.com" & Offline == "No", "type" := "Non-Taxed Non-Amazon"]

# Getting months between present and collection date
full_data[, ':=' (date = as.Date(paste0(year, "-", month, "-01")),
                  AmazonCollected = as.Date(AmazonCollected, format = "%Y-%m-%d"))]
full_data[, "lapse" := as.integer(round(difftime(date, AmazonCollected, units = "days") / 30))]

# Collapsing by months and type
monthlySpending <- full_data[, .(spending = sum(prod_totprice_real)), by = .(lapse, type, state, machine_id)]
eventStudy <- monthlySpending[, .(spending = mean(spending)), by = .(lapse, type)]

# Plotting event study
chart <- plot_ly(data = eventStudy[lapse >= -60 & lapse <= 60], x = ~lapse) %>%
  add_lines(y = ~spending, split = ~type, line = list(width = 5)) %>%
  layout(title = "Average Monthly Spending",
         titlefont = list(size = 35),
         xaxis = list(title = "Months Until Amazon Collection", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Monthly Spending", range = c(0, 100), dtick = 20,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: comScore. News Reports.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/eventStudy.png")
