# Loading data
library(data.table)
library(readxl)
library(Quandl)
load('/home/mallick/Desktop/comScore/Transactions.rda')

# Restricting to Amazon categories and transactions between 2006-2014
amazon_categories <- c(1:40, 54:56)
start_count <- nrow(transactions)
transactions <- transactions[year >= 2006 & year <= 2014 & prod_category_id %in% amazon_categories]
end_count <- nrow(transactions)
start_count - end_count # Number of transactions removed
rm(amazon_categories, start_count, end_count)

# Generating Amazon dummy
transactions[, 'amazon' := ifelse(domain_name == 'amazon.com', 1, 0)]
setkey(transactions, machine_id, year)

# Merging with demographics (all transactions can be matched)
load('/home/mallick/Desktop/comScore/Demographics.rda')
setkey(demographics, machine_id, year)
fullData <- merge(transactions, demographics)
setkey(fullData, year, zip_code)
rm(demographics, transactions)

# Dropping households with ZIP codes of 99999 because they are invalid
start_count <- nrow(fullData)
fullData <- fullData[zip_code != 99999]
end_count <- nrow(fullData)
start_count - end_count # Number of transactions removed

# Merging with ZIP data (only 350 transactions cannot be matched)
load('/home/mallick/Desktop/comScore/zip_tax.rda')
setkey(zipTax, year, zip_code)
start_count <- nrow(fullData)
fullData <- merge(fullData, zipTax)
end_count <- nrow(fullData)
start_count - end_count
setkey(fullData, state)
rm(zipTax)

# Adding state names
states <- data.table(state = c(state.abb, 'DC'),
                     stateName = c(state.name, 'District of Columbia'))
setkey(states, state)
fullData <- merge(fullData, states)
setkey(fullData, zip_code)
rm(states)

# # Adding county names (13,623 could not be matched to 2014 county names)
# zip_county <- fread("/home/mallick/Desktop/comScore/zipState2014.csv",
#                     select = c('zcta5', 'county14'))
# zip_county <- zip_county[-1]
# setnames(zip_county, c('zip_code', 'county'))
# zip_county$zip_code <- as.numeric(zip_county$zip_code)
# zip_county$county <- as.numeric(zip_county$county)
# zip_county <- zip_county[zip_code != 99999]
# zip_county <- unique(zip_county, by = 'zip_code')
# setkey(zip_county, zip_code)
# start_count <- nrow(fullData)
# fullData <- merge(fullData, zip_county)
# end_count <- nrow(fullData)
# start_count - end_count
# rm(zip_county)
# setkey(fullData, stateName)

# Adding in sales tax collection dates
taxDates <- setDT(read_excel(path = './Research/OnlineShopping/AmazonLaws.xls', sheet = 'Data'))
taxDates <- fread('/home/mallick/Dropbox/Research/OnlineShopping/AmazonLaws.csv',
                  select = c('State', 'DateCollected'))
taxDates$DateCollected <- as.Date(taxDates$DateCollected, format = '%y/%m/%d')
setnames(taxDates, c('stateName', 'collectDate'))
setkey(taxDates, stateName)
start_count <- nrow(fullData)
fullData <- merge(fullData, taxDates)
end_count <- nrow(fullData)
start_count - end_count
rm(taxDates)

# Setting sales tax indicator
fullData[, 'date' := as.Date(paste(year, month, '01', sep = '-'))]
fullData[, 'collect' := ifelse(event_date >= collectDate, 1, 0)]
fullData$collect <- ifelse(is.na(fullData$collect), 0, fullData$collect)

# Deflating to real prices
cpi <- setDT(Quandl('FRED/CPIAUCSL', start_date = '2006-01-01'))
setnames(cpi, c('date', 'cpi'))
setkey(cpi, date)
setkey(fullData, date)
fullData <- merge(fullData, cpi)
fullData[, c('realProdPrice', 'realBasketPrice') :=
           .(prod_totprice / cpi * 100, basket_tot / cpi * 100)]
rm(cpi)

# Adding monthYear fixed effect dummy
fullData[, 'monthYear' := (year - 2006) * 12 + month]

# Generating tau
fullData[, 'tau' := amazon * collect * ave_tax + (1 - amazon) * ave_tax]
save(fullData, file = '../Desktop/comScore/katjaTable6.rda', compress = TRUE)

# Doing regression (linear probability)
library(data.table)
load('./NewComScore/Data/katjaTable6.rda')
katjaReg <- lm(amazon ~ log(1 + tau) + factor(state) + factor(prod_category_id) + factor(monthYear), data = fullData)
summary(katjaReg)

# Doing regression (Amazon expenditures)
reg2_data <- fullData[amazon == 1, .(log_exp = log(sum(prod_totprice))), keyby = .(county, year, collect, state)]
katjaReg2 <- lm(log_exp ~ collect + factor(state) + factor(year), data = reg2_data[log_exp >= 0])

