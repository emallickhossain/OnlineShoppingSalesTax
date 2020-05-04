library(data.table)
library(Quandl)
library(plotly)
Quandl.api_key("mxJN5yLSyrtAMPu8rwiZ")
start_date <- "2000-01-01"
end_date <- "2017-06-30"

# This makes chart of total real retail and online spending
retail <- setDT(Quandl(c("FRED/RSXFS", "FRED/ECOMSA"),
                       start_date = start_date, end_date = end_date))
setnames(retail, c("date", "retailExFood", "eCom"))
quarterRetail <- retail[, lapply(.SD, sum, na.rm = TRUE), by = .(date = as.yearqtr(date))]

# Getting CPI to deflate to real values
cpi <- setDT(Quandl(c("FRED/CPIAUCSL"),
                    start_date = start_date, end_date = end_date))
setnames(cpi, c("date", "cpi"))
quarterCPI <- cpi[, lapply(.SD, mean, na.rm = TRUE), by = .(date = as.yearqtr(date))]

# Getting recession
recession <- setDT(Quandl(c("FRED/USREC"), start_date = start_date, end_date = end_date))
setnames(recession, c("date", "recession"))

# Combining and deflating
retail <- merge(quarterCPI, quarterRetail, by = "date")
retail <- retail[, .(realRetailExFood = retailExFood / cpi * 100,
                     realECom = eCom / cpi * 100,
                     date = as.Date(date))]

# Converting to billions of dollars
retail <- retail[, c("realRetailExFood", "realECom") :=
                   .(realRetailExFood / 1000, realECom / 1000)]

# Plotting
chart <- plot_ly(data = retail, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realRetailExFood, name = "Total Retail (left)", line = list(width = 10)) %>%
  add_lines(y = ~realECom, name = "Online (right)", yaxis = "y2",
            line = list(dash = "dot", width = 10)) %>%
  add_lines(data = recession, x = ~date, y = ~recession * 6000, yaxis = "y2",
            line = list(width = 0),
            fill = "tozerox",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = "Real Retail and Online Sales (2000-2017)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25), nticks = 4),
         yaxis = list(title = "Total Retail Sales", range = c(0, 600),
                      dtick = 100,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick dtick so that ticks line up on both sides of chart
         yaxis2 = list(title = "Online Sales", range = c(0, 60),
                       dtick = 10,
                       side = "right", overlaying = "y",
                       titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", y = 1.02, x = 0, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 140, r = 140, t = 70, b = 150, pad = 10),
         annotations = list(text = paste0("Source: U.S. Census Bureau<br>Note: ",
                                          "Sales are in billions of 1982-1984 dollars."),
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.25))
export(chart, file = "../5_figures/retailOnlineLevels.png")
