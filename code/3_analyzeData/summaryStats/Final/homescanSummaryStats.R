# Produces table of Homescan summary statistics
library(data.table)
library(knitr)
library(Hmisc)
library(forcats)
library(ggplot2)
library(ggthemes)
library(stringr)
threads <- 8
panel <- fread("/scratch/upenn/hossaine/comScore/NielsenFullPanel.csv",
               select = c("projection_factor", "household_income", "panel_year",
                          "age", "college", "household_code", "married",
                          "household_size", "child"))

panel[, "household_income" := factor(household_income,
                                     levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16,
                                                17, 18, 19, 21, 23, 26, 27),
                                     labels = c(2.5, 6.5, 9, 11, 13.5, 17.5,
                                                22.5, 27.5, 32.5, 37.5, 42.5,
                                                47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_income" := as.numeric(as.character(household_income))]
panel <- panel[!is.na(household_income)]

# Getting number and tenure of households
tenure <- panel[, .(tenure = uniqueN(panel_year)), by = household_code]
tenure[, .(mean(tenure), median(tenure))]
uniqueN(panel$household_code)
panel[, c("household_code", "panel_year") := NULL]

# Generating summary stats table
summaryMeans <- panel[, lapply(.SD, weighted.mean, w = projection_factor)]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, wtd.var, w = projection_factor)]
summarySD <- sqrt(summarySD)
summarySD[, "type" := "SD"]
summary25 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.25)]
summary25[, "type" := "25th Pctile"]
summary75 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.75)]
summary75[, "type" := "75th Pctile"]

finalTable <- rbindlist(list(summaryMeans, summarySD, summary25, summary75), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = "type")
finalTableWide <- dcast(finalTableLong, variable ~ type)
setnames(finalTableWide, "variable", "Variable")
setcolorder(finalTableWide, c("Variable", "Mean", "SD", "25th Pctile" ,"75th Pctile"))
finalTableWide <- finalTableWide[Variable != "projection_factor"]
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "child", "Variable" := "Child Present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "markdown")
# saved as homescanSummaryStats.tex
