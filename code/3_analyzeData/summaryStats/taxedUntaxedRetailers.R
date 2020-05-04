# Getting taxed and untaxed retailers
library(data.table)
load('../Desktop/comScore/Transactions.rda')

# Limiting data to Amazon categories and high sales websites
transactions <- transactions[prod_category_id %in% c(1:40, 54:60, 99) & year > 2002]
domainSales <- transactions[, .(totalSales = sum(prod_totprice, na.rm = TRUE)), by = domain_name]

# Looking at quantiles of sales, top ~25% of sites account for over 95% of sales
quantiles(domainSales$totalSales)

# Taking only sites with >$50k in total sales
domainSales <- domainSales[totalSales >= 50000]

# Snagging unique domains
fwrite(domainSales, file = './Research/OnlineShopping/onlineOffline.csv')
