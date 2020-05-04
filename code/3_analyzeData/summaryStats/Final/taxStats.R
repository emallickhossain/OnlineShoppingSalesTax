# Generates map and histogram of tax rates
library(data.table)
library(ggplot2)
library(ggthemes)
library(maps)

# Getting tax data and restricting to December 2016
tax <- fread("./code/0_data/Clean/zip_tax_min.csv")

# Matching with county names
cty_data <- setDT(merge(county.fips, tax[year == 2016 & month == 12], by = "fips"))
cty_data[, "taxCut" := cut(sales_tax, breaks = c(0, 0.05, 0.06, 0.07, 0.08, 0.12),
                           include.lowest = TRUE)]
cty_data[, c("region", "subregion") := tstrsplit(polyname, ",", fixed = TRUE)]
cty_data <- setDT(merge(cty_data, map_data("county"), by = c("region", "subregion")))

ggplot(data = cty_data, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = taxCut), color = NA) +
  geom_polygon(color = "black", fill = NA, size = 0.1) +
  theme_tufte() +
  labs(fill = "Sales Tax") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("./code/5_figures/taxRateByCounty.pdf", width = 6, height = 4)

# Making histogram of tax rates
ggplot(data = tax, aes(x = sales_tax)) +
  geom_histogram(aes(y = ..density..)) +
  theme_tufte() +
  xlab("Sales Tax Rate") +
  ylab("Density")
ggsave("./code/5_figures/taxHistogram.pdf", width = 6, height = 4)

# Making histogram of tax differences
tax[, "tax_diff" := sales_tax - min_adj_tax]
ggplot(tax, aes(x = tax_diff)) +
  geom_histogram(aes(y = ..density..)) +
  theme_tufte() +
  xlab("Sales Tax Difference") +
  ylab("Density")
