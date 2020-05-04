# This takes the TDS data and puts it into one file. I only get the total sales tax
# and total use tax for each ZIP code and city. I append the year and the month as well
# I'll decide if I want to use the min or max sales tax later.
library(data.table)
library(stringr)
library(purrr)
library(tidyr)

year <- c(rep(2006:2013, each = 12), rep(2014, 5), rep(2015:2016, each = 4))
month <- c(rep(1:12, 8), 1:5, rep(c(1, 4, 7, 10), 2))

loadTaxes <- function(year, month) {
  if (year %in% 2006:2007) {
    suffix <- " AS_complete/AS_complete.txt"
  } else if (year %in% c(2008:2014)) {
    suffix <- " AS_completeplus/AS_complete+.txt"
  } else if (year %in% 2015:2016) {
    suffix <- " AS_basicIIplus/AS_basicII+.txt"
  }
  data <- fread(paste0("/home/mallick/Desktop/Research/RawData/taxData/", year, "/",
                       str_pad(month, 2, "left", "0"), substr(year, 3, 4), suffix),
                select = c("ZIP_CODE", "STATE_ABBREV", "TOTAL_SALES_TAX"))
  data[, c("year", "month") := .(year, month)]
  setnames(data, c("zip_code", "state", "sales_tax", "year", "month"))
  return(data)
}

fullTaxData <- rbindlist(map2(year, month, loadTaxes))

# Add in missing months
dates <- seq.Date(from = as.Date("2014-01-01"), to = as.Date("2016-12-01"), by = "month")
zips <- unique(fullTaxData$zip_code)
full_dates <- setDT(expand.grid(zips, dates))
setnames(full_dates, c("zip_code", "date"))
full_dates[, c("year", "month", "date") :=
             .(as.integer(year(date)), as.integer(month(date)), NULL)]
fullTaxData <- merge(fullTaxData, full_dates, keyby = c("zip_code", "year", "month"),
                     all = TRUE)

# Filling down missing sales taxes and getting minimum tax rates
fullTaxData <- setDT(unique(fill(fullTaxData, state, sales_tax, .direction = "down")))
fullTaxDataMin <- fullTaxData[, .(sales_tax = min(sales_tax)), by = .(year, month, state, zip_code)]

# Getting adjacent tax rate ----------------------------------------------------
# Getting column names for use later
cols <- paste0("sales_tax", sort(rep(2006:2016, 12)), "_", 1:12)

# Load county adjacency data from 2010 Census (https://www.census.gov/geo/reference/county-adjacency.html)
county_adj <- fread("./code/0_data/census_2010_county_adjacency.txt",
                    col.names = c("county", "own_fips", "neighbor_county", "neighbor_fips"))
county_adj <- setDT(fill(county_adj, own_fips))

# Adding in ZIP code and collapsing to ZIP
zipFip <- fread(paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/",
                       "zcta_county_rel_10.txt"),
                select = c("ZCTA5", "STATE", "COUNTY", "COPOPPCT"))
zipFip[, "fips" := paste0(str_pad(STATE, 2, "left", "0"),
                          str_pad(COUNTY, 3, "left", "0"))]
zipFip <- zipFip[, .SD[which.max(COPOPPCT)], by = ZCTA5]
zipFip[, c("STATE", "COUNTY", "COPOPPCT") := NULL]
setnames(zipFip, "ZCTA5", "zip_code")

# Getting FIPS codes for each zip code and then averaging by FIPS code
zipTax <- merge(fullTaxDataMin, zipFip, by = c("zip_code"))
zipTax <- zipTax[, .(sales_tax = mean(sales_tax)), by = .(year, month, fips)]
zipTax[, "fips" := as.integer(fips)]
zipTaxWide <- dcast(zipTax, fips ~ year + month, value.var = "sales_tax")
setnames(zipTaxWide, c("fips", cols))

# Merging with county adjacencies and getting minimum adjacent tax rate
neighbor_tax <- merge(county_adj, zipTaxWide, by.x = "neighbor_fips", by.y = "fips")
setcolorder(neighbor_tax, c("own_fips", "county", "neighbor_fips", "neighbor_county", cols))
min_neighbor_tax <- neighbor_tax[, lapply(.SD, min), .SDcols = cols, keyby = own_fips]

# Melting to long form
min_neighbor_tax_long <- melt(min_neighbor_tax, id.vars = c("own_fips"),
                              measure = patterns("^sales_tax"),
                              value.name = "min_sales_tax",
                              variable.name = "year")
min_neighbor_tax_long[, c("year", "month") :=
                        .(as.integer(substr(year, 10, 13)),
                          as.integer(substr(year, 15, 16)))]
setnames(min_neighbor_tax_long, c("fips", "year", "min_adj_tax", "month"))

# Final Tax data
final_zip_tax <- merge(zipTax, min_neighbor_tax_long, by = c("year", "month", "fips"))
fwrite(final_zip_tax, "./code/0_data/Clean/zip_tax_min.csv")
