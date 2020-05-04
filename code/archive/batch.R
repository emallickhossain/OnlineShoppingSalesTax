# ------------------ ABOUT------------------------------------------------------
# Full replication script that collects, cleans, and analyzes the data
setwd("/home/mallick/Dropbox/Research/OnlineShopping/OnlineShoppingSalesTax/code/")

# ------------------ Step 1: Get Data ------------------------------------------
source("./1_getData/getTransactions.R") #yes
source("./1_getData/getDemographics.R") #yes
source("./1_getData/getTaxData.R")
source("./1_getData/getTransactionBrowsing.R")

# ------------------ Step 2: Clean Data ----------------------------------------
source("./2_cleanData/cleanTransactions.R") #yes
source("./2_cleanData/cleanDemographics.R") #yes
source("./2_cleanData/cleanTaxData.R")

# ------------------ Step 3: Combine Data --------------------------------------
source("./3_combineData/get_min_tax_rate.R")
source("./3_combineData/combineData.R")

# ------------------ Step 4: Run Regressions -----------------------------------
source("./4_analyzeData/Regressions/runRegressionsHouseholdAmazonSpending.R")
source("./4_analyzeData/Regressions/runRegressionsCountyAmazonSpending.R")
source("./4_analyzeData/Regressions/runRegressionsKatja.R")

# ------------------ Step 5: Make Regression Tables ----------------------------
source("./4_analyzeData/Tables/regressionTablesHouseholdAmazonSpending.R")
source("./4_analyzeData/Tables/regressionTablesCountyAmazonSpending.R")
source("./4_analyzeData/Tables/regressionTablesKatja.R")

# ------------------ Step 6: Make Charts ---------------------------------------
source("./4_analyzeData/Figures/retailECommerce.R") #yes
source("./4_analyzeData/Figures/eCommerceCatalog.R") #yes
source("./4_analyzeData/Figures/unique_transaction_domains.R") #yes
source("./4_analyzeData/Figures/eCommerceDecomposition.R") #yes
source("./4_analyzeData/Figures/hhi.R") #yes

