# This generates the chart of browsing on retail websites
library(data.table)
library(ggplot2)
load("/home/mallick/Desktop/comScore/comScoreBrowsingRegData.rda")

# Cleaning
full_data <- full_data[monthlyDuration >= 0]
bad_domains <- c("google.com", "yahoo.com", "atdmt.com", "msn.com", "aol.com", "go.com",
                 "googlesyndication.com-o02", "microsoft.com", "weatherbug.com",
                 "bankofamerica.com", "netflix.com", "earthlink.com", "chase.com",
                 "wellsfargo.com", "match.com", "usatoday.com", "earthlink.net",
                 "mlb.com", "budget.com", "bluemountain.com", "delta.com", "nationalcar.com",
                 "cheaptickets.com", "capitalone.com", "schwab.com", "taxact.com",
                 "ancestry.com", "ticketsnow.com", "internet-taxprep.com",
                 "netflix.com", "earthlink.net", "dollar.com", "napster.com",
                 "united.com", "hrblock.com", "date.com", "letstalk.com",
                 "bankofamerica.com", "ftd.com", "daysinn.com", "taxactonline.com",
                 "taxbrain.com", "usaa.com", "completetax.com", "discovercard.com",
                 "tickets.com", "equifax.com", "comcast.com", "proflowers.com",
                 "xmradio.com", "1800flowers.com", "jetblue.com", "freetaxusa.com",
                 "perfectmatch.com", "esmarttax.com", "starwoodhotels.com",
                 "linkedin.com", "ronssmokeshop.com", "pensketruckrental.com",
                 "securemingle.com", "cig4u.com", "wingateinns.com", "knightsinn.com",
                 "kycigarettes.com", "chemistry.com", "bigjoesmokeshop.com",
                 "mate1.com", "indiansmokesonline.com", "cigarettessentdirect.com",
                 "cigarettesbyinternet.com", "allofourbutts.com", "silvercloudsmokeshop.com",
                 "areasmoke.com", "cigsales.com", "cheap-cig.com", "livenation.com",
                 "booking.com", "simplysmoke.com", "123smoke.com", "cheap-smokes.biz",
                 "livingsocial.com", "azcigs.com", "groupon.com", "zynga.com",
                 "securecigs.com", "paylessmoke.com", "k2smokes.com", "buycheapcigarettes.com",
                 "cigarettesavers.com", "1stcommerce.net", "upinsmokes.com", "buydiscountcigarettes.com",
                 "mailordercigarettes.biz", "eztobacco.com", "smokinprices.com",
                 "dirtcheapcigs.com", "mycigarettes.com", "pnc.com", "havecigs.com",
                 "tdbank.com", "budgettruck.com", "cigarettesamerica.com", "smokesignals.com",
                 "thompsoncigar.com", "usbank.com", "ticketmaster.ca", "blackpeoplemeet.com",
                 "papajohnsonline.com", "pizzahut.com", "relationshipexchange.com",
                 "ticketweb.com", "fandango.com", "stubhub.com", "taxslayer.com",
                 "eztaxreturn.com", "taxcut.com", "chase.com", "expresstaxrefund.com",
                 "ameritrade.com", "usps.com", "americansingles.com", "wellsfargo.com",
                 "match.com", "ups.com", "regions.com", "movietickets.com", "weatherbug.com")
full_data <- full_data[!domain_name %in% bad_domains]

# Average Browsing Chart (Full Sample)
monthlyBrowsing <- full_data[, .(monthlyDuration = sum(monthlyDuration) / 60 / uniqueN(machine_id)),
                             by = .(year, month)]
monthlyBrowsing[, "date" := as.Date(paste0(year, "-", month, "-01"))]
chart <- plot_ly(data = monthlyBrowsing, x = ~date, y = ~monthlyDuration, mode = "lines",
                 width = 1200, height = 800, type = "scatter", name = "Total") %>%
  layout(title = "Average Shopping Browsing",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c("2008-01-01", "2017-12-31")),
         yaxis = list(title = "Hours", range = c(0, 3), dtick = 1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 100, pad = 10),
         annotations = list(text = "Source: comScore.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.15))
export(chart, file = "./code/5_figures/averageShoppingBrowsing.png")

# Histogram Browsing Chart
monthlyBrowsing <- full_data[, .(monthlyDuration = sum(monthlyDuration) / 60),
                             by = .(year, machine_id)]
chart <- plot_ly(data = monthlyBrowsing, x = ~monthlyDuration, type = "histogram",
                 width = 1200, height = 800) %>%
  layout(title = "Histogram of Monthly Shopping Browsing",
         titlefont = list(size = 35),
         xaxis = list(title = "Hours", titlefont = list(size = 30),
                      tickfont = list(size = 25), range = c(0, 100)),
         yaxis = list(title = "Density", range = c(0, 100000), dtick = 10000,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 100, pad = 10),
         annotations = list(text = "Source: comScore.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.15))
export(chart, file = "./code/5_figures/averageShoppingHist.png")
