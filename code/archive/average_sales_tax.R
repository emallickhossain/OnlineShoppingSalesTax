library(data.table)
library(plotly)
# Getting sales taxes collected by states (downloaded from Census at https://www.census.gov/govs/statetax/historical_data.html)
# 'Retail sales' and 'retail sales taxes' data is from the Annual Retail Trade
# report (downloaded from https://www.census.gov/retail/index.html)

# The retail sales tax is defined by Census as follows:
# Sales Tax - A tax collected by retailers and certain service providers when they make taxable retail sales. Sales taxes could include state, county and local taxes. Sales taxes exclude excise taxes and occupancy taxes for accommodation industry.
tax_table <- data.table(tax_collections = c(284843903, 272165271, 260019042,
                                            249155625, 237688541, 224314387,
                                            226781684, 240165120, 238303540,
                                            229630169, 212920979, 197948848,
                                            184596707, 179665257, 179318645,
                                            174461160, 164378016, 155272097,
                                            147413533, 139363248, 132236159,
                                            123005615, 114634988, 108712128),
                        year = 2015:1992,
                        retail_sales = c(4727427, 4639440, 4458450, 4302229,
                                         4102952, 3818048, 3612471, 3935315,
                                         3995182, 3871573, 3689283, 3473048,
                                         3262731, 3128552, 3062268, 2983276,
                                         2803090, 2581762, 2468767, 2361549,
                                         2217616, 2105235, 1937628, 1811237),
                        retail_sales_taxes = c(168863, 162586, 155978, 149284,
                                               144664, 138653, 132692, 141046,
                                               146447, 141335, 135985, 127833,
                                               rep(NA, 12)))

# Readjusting to dollars since the tax data is in thousands of dollars and
# retail sales is in millions of dollars
tax_table[, c('tax_collections', 'retail_sales', 'retail_sales_taxes') :=
            .(tax_collections * 1000, retail_sales * 1000000, retail_sales_taxes * 1000000)]

# Generating average sales tax rates
tax_table[, c('avg_sales_tax', 'avg_sales_tax_trade') :=
            .(tax_collections / retail_sales * 100, retail_sales_taxes / retail_sales * 100)]

# Plotting
plot_ly(data = tax_table, x = ~year) %>%
  #add_trace(y = ~avg_sales_tax, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~avg_sales_tax_trade, type = 'scatter', mode = 'lines')

plot_ly(data = tax_table, x = ~year) %>%
  add_trace(y = ~retail_sales, type = 'scatter', mode = 'lines')
  #add_trace(y = ~retail_sales_taxes, type = 'scatter', mode = 'lines')
