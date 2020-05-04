# This gets all the shopping trips together
library(lubridate)
library(data.table)

full_household_spending <- NULL
for (i in 2004:2015) {
  trips <- fread(paste0('/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/',
                        i, '/Annual_Files/trips_', i, '.tsv'),
                 select = c('household_code', 'purchase_date', 'retailer_code',
                            'panel_year', 'total_spent'))

  # Making coarser categories
  trips[, 'store_cat' := 'Offline']
  setindex(trips, retailer_code)
  trips[retailer_code <= 3999, 'store_cat' := 'Grocery']
  trips[retailer_code %in% 4700:4849, 'store_cat' := 'Online']

  # Adding in possibilities for Amazon (top 3 online spending retailers)
  trips[retailer_code == 4804, 'store_cat' := 'Amazon1']
  trips[retailer_code == 4826, 'store_cat' := 'Amazon2']
  trips[retailer_code == 4849, 'store_cat' := 'Amazon3'] #catch-all category
  trips[, 'purchase_date' := as.Date(fast_strptime(purchase_date, format = '%Y-%m-%d'))]
  trips[, c('month', 'year') := .(as.integer(month(purchase_date)),
                                  as.integer(year(purchase_date)))]

  # Because of how Nielsen starts panel, I drop all purchases from
  # December of the previous year
  household_spending <- trips[year == panel_year,
                              .(spending = sum(total_spent), trips = .N),
                              by = .(year, month, household_code, store_cat)]
  full_household_spending <- rbind(full_household_spending, household_spending)
  rm(trips, household_spending)
}

save(full_household_spending,
     file = '/home/mallick/Desktop/Nielsen/Data/HouseholdSpending.rda',
     compress = TRUE)
rm(full_household_spending)
