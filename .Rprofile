# -------------- FRED API -------------------------------------------------------
fredAPI <- "e2b57235e981bb9db50b789104012b3b"

# -------------------- WRDS setup ----------------------------------------------
library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='hossaine')
