library(data.table)
load('/home/mallick/Desktop/Nielsen/Data/fullPanel.rda')
full_panel[, 'internet' := internet - 1]
glm(internet ~ factor(income) + factor(race) + state + factor(age),
    family = binomial(link = 'probit'), weights = full_panel$projection_factor,
    data = full_panel)
