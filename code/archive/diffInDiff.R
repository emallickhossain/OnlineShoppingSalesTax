# This runs the diff-in-diff regression for households that were in the panel
# on both sides of the Amazon tax flip.

library(data.table)
library(lfe)
library(plotly)
load('/home/mallick/Desktop/comScore/regressionDataNoTax.rda')

# Finding households that lived through the Amazon tax change. This removes households
# for which the indicator of whether Amazon collected tax was always 1 or always 0.
full_data[, 'to_keep' := mean(amazon_collect), by = .(machine_id, state)]
transition <- full_data[to_keep != 0.0 & to_keep != 1.0]

# # -------------- Doing diffs for spending on transition households -------------
# # Total spending
# transition_spending <- transition[, .(monthly_spending = sum(prod_totprice)),
#                                   by = .(year, month, machine_id, state, amazon_collect)]
# # Amazon spending
# transition_spending_amazon <- transition[amazon == 1,
#                                          .(monthly_spending = sum(prod_totprice)),
#                                          by = .(year, month, machine_id,
#                                                 state, amazon_collect)]
# # non-Amazon taxed spending
# transition_spending_taxed <- transition[amazon == 0 & taxed == 1,
#                                         .(monthly_spending = sum(prod_totprice)),
#                                         by = .(year, month, machine_id,
#                                                state, amazon_collect)]
# # non-Amazon untaxed
# transition_spending_untaxed <- transition[amazon == 0 & taxed == 0,
#                                           .(monthly_spending = sum(prod_totprice)),
#                                           by = .(year, month, machine_id,
#                                                  state, amazon_collect)]
#
# # Running the DD regression with state FEs only on transition households
# reg1 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = transition_spending)
# reg2 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = transition_spending_amazon)
# reg3 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = transition_spending_taxed)
# reg4 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = transition_spending_untaxed)
#
# # -------------- Doing diffs for spending on all households -------------
# # Total spending
# full_spending <- full_data[, .(monthly_spending = sum(prod_totprice)),
#                            by = .(year, month, machine_id, state, amazon_collect)]
# # Amazon spending
# full_spending_amazon <- full_data[amazon == 1,
#                                   .(monthly_spending = sum(prod_totprice)),
#                                   by = .(year, month, machine_id,
#                                          state, amazon_collect)]
# # non-Amazon taxed spending
# full_spending_taxed <- full_data[amazon == 0 & taxed == 1,
#                                  .(monthly_spending = sum(prod_totprice)),
#                                  by = .(year, month, machine_id,
#                                         state, amazon_collect)]
# # non-Amazon untaxed
# full_spending_untaxed <- full_data[amazon == 0 & taxed == 0,
#                                    .(monthly_spending = sum(prod_totprice)),
#                                    by = .(year, month, machine_id,
#                                           state, amazon_collect)]
#
# # Running the DD regression with state FEs only on all households
# reg5 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = full_spending)
# reg6 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = full_spending_amazon)
# reg7 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = full_spending_taxed)
# reg8 <- felm(monthly_spending ~ amazon_collect:state |
#                state + month,
#              data = full_spending_untaxed)

# ----------------------- TOTAL ONLINE SPENDING --------------------------------
# Making charts of state spending before and after change (at change is indexed to 100)
full_data[, 'change_month_counter' := ifelse(amazon_collect == 1, month_counter, 999), by = state]
full_data[, 'change_month_counter' := min(change_month_counter), by = state]
full_data[, 'relative_to_change' := month_counter - change_month_counter]
with_changes <- full_data[change_month_counter != 999, .(monthly_spending = sum(prod_totprice)),
                          keyby = .(state, relative_to_change)]
base_spending <- with_changes[relative_to_change == 0, .(base_year = monthly_spending), by = state]
with_changes <- merge(base_spending, with_changes, by = 'state')
with_changes[, 'index_spending' := monthly_spending / base_year * 100, by = state]
setkey(with_changes, state, relative_to_change)

make_graph <- with_changes[, .(avg_index = mean(index_spending),
                               avg_spending = mean(monthly_spending)), keyby = relative_to_change]
plot_ly(data = make_graph, x = ~relative_to_change, y = ~avg_spending, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Online Spending')
export(file = '/home/mallick/Downloads/Rplot1.png')
plot_ly(data = make_graph, x = ~relative_to_change, y = ~avg_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Online Spending (Spending at change = 100)')
export(file = '/home/mallick/Downloads/Rplot2.png')

# ----------------------- TOTAL AMAZON SPENDING --------------------------------
# Making charts of state spending before and after change (at change is indexed to 100)
with_changes_amazon <- full_data[change_month_counter != 999 & amazon == 1,
                                 .(monthly_spending = sum(prod_totprice)),
                                 keyby = .(state, relative_to_change)]
base_spending_amazon <- with_changes_amazon[relative_to_change == 0, .(base_year = monthly_spending), by = state]
with_changes_amazon <- merge(base_spending_amazon, with_changes_amazon, by = 'state')
with_changes_amazon[, 'index_spending' := monthly_spending / base_year * 100, by = state]
setkey(with_changes_amazon, state, relative_to_change)

make_graph_amazon <- with_changes_amazon[, .(avg_index = mean(index_spending),
                                             avg_spending = mean(monthly_spending)),
                                         keyby = relative_to_change]
plot_ly(data = make_graph_amazon, x = ~relative_to_change, y = ~avg_spending, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Amazon Spending')
export(file = '/home/mallick/Downloads/Rplot3.png')

plot_ly(data = make_graph_amazon, x = ~relative_to_change, y = ~avg_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Amazon Spending (Spending at change = 100)')
export(file = '/home/mallick/Downloads/Rplot4.png')

# ----------------------- TOTAL NON-AMAZON SPENDING --------------------------------
# Making charts of state spending before and after change (at change is indexed to 100)
with_changes_nonamazon <- full_data[change_month_counter != 999 & amazon == 0,
                                    .(monthly_spending = sum(prod_totprice)),
                                    keyby = .(state, relative_to_change)]
base_spending_nonamazon <- with_changes_nonamazon[relative_to_change == 0, .(base_year = monthly_spending), by = state]
with_changes_nonamazon <- merge(base_spending_nonamazon, with_changes_nonamazon, by = 'state')
with_changes_nonamazon[, 'index_spending' := monthly_spending / base_year * 100, by = state]
setkey(with_changes_nonamazon, state, relative_to_change)

make_graph_nonamazon <- with_changes_nonamazon[, .(avg_index = mean(index_spending),
                                             avg_spending = mean(monthly_spending)),
                                         keyby = relative_to_change]
plot_ly(data = make_graph_nonamazon, x = ~relative_to_change, y = ~avg_spending, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Non-Amazon Spending')
export(file = '/home/mallick/Downloads/Rplot5.png')

plot_ly(data = make_graph_nonamazon, x = ~relative_to_change, y = ~avg_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Non-Amazon Spending (Spending at change = 100)')
export(file = '/home/mallick/Downloads/Rplot6.png')

# ----------------------- TOTAL NON-TAXED, NON-AMAZON SPENDING --------------------------------
# Making charts of state spending before and after change (at change is indexed to 100)
with_changes_nontaxed <- full_data[change_month_counter != 999 & amazon == 0 & taxed == 0,
                                    .(monthly_spending = sum(prod_totprice)),
                                    keyby = .(state, relative_to_change)]
base_spending_nontaxed <- with_changes_nontaxed[relative_to_change == 0, .(base_year = monthly_spending), by = state]
with_changes_nontaxed <- merge(base_spending_nontaxed, with_changes_nontaxed, by = 'state')
with_changes_nontaxed[, 'index_spending' := monthly_spending / base_year * 100, by = state]
setkey(with_changes_nontaxed, state, relative_to_change)

make_graph_nontaxed <- with_changes_nontaxed[, .(avg_index = mean(index_spending),
                                                   avg_spending = mean(monthly_spending)),
                                               keyby = relative_to_change]
plot_ly(data = make_graph_nontaxed, x = ~relative_to_change, y = ~avg_spending, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Non-Amazon, Non-Taxed Spending')
export(file = '/home/mallick/Downloads/Rplot7.png')

plot_ly(data = make_graph_nontaxed, x = ~relative_to_change, y = ~avg_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Non-Amazon, Non-Taxed Spending (Spending at change = 100)')
export(file = '/home/mallick/Downloads/Rplot8.png')

