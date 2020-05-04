# This makes chart of total real retail and online spending
library(data.table)
library(purrr)
library(ggplot2)
library(ggthemes)
library(fredr)
fredr_set_key(fredAPI)
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2017-12-31")

# retail <- rbindlist(map(c("ECOMPCTSA", "USRECQ"), fredr, observation_start = start_date))
retail <- rbindlist(map("ECOMPCTSA", fredr, observation_start = start_date))
rec <- fread("./code/0_data/recession.csv", colClasses = c("Date", "Date"))

# Plotting
ggplot() +
  geom_line(data = retail, aes(x = date, y = value)) +
  geom_rect(data = rec, aes(xmin = start, xmax = end, ymin = 0, ymax = 12),
            fill = "gray", alpha = 0.2) +
  labs(x = "Year", y = "Percent") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()

ggsave(filename = "./code/5_figures/retailOnline.pdf", height = 4, width = 6)
