# Gets non-grocery products from Nielsen
library(data.table)
library(purrr)
library(stringr)
fileName <- "/home/mallick/Desktop/Nielsen/Data/CleanComScore/"

# Loading data
panel <- fread(paste0(fileName, "fullPanel.csv"))

getPurchases <- function(year) {
  print(year)
  purchases <- fread(paste0(fileName, "Purchases/purchase", year, ".csv"),
                     select = c("trip_code_uc", "total_price_paid"))
  purchases <- purchases[, .(full_price = sum(total_price_paid)), by = .(trip_code_uc)]

  trips <- fread(paste0(fileName, "Trips/trips", year, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code", "purchase_date"))
  trips[, c("month", "purchase_date") := .(as.integer(substr(purchase_date, 6, 7)), NULL)]

  # Merging with trips data
  trips <- merge(purchases, trips, by = "trip_code_uc")
  trips[, "onlineSpending" := ifelse(retailer_code %in% 4700:4849, full_price, 0)]
  trips <- trips[, .(totalSpending = sum(full_price),
                     onlineSpending = sum(onlineSpending)),
                 by = .(household_code, panel_year, month)]
  trips[, "offlineSpending" := totalSpending - onlineSpending]
  return(trips)
}
householdSpending <- rbindlist(map(2004:2016, getPurchases))

# Merging with household data
householdSpending <- merge(householdSpending, panel, by = c("household_code", "panel_year"))

fwrite(householdSpending, paste0(fileName, "nielsenAll.csv"))
