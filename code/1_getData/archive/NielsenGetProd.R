# Get product data
# Drops all magnet data
# Only keeps departments 0, 7, 9 (non-grocery items)
library(data.table)
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"

prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "")
prod[, "nonGroc" := ifelse(department_code %in% c(0, 7, 9), 1L, 0L) ]
prod <- prod[!product_module_code %in% 445:468]
prod <- prod[, .(upc, upc_ver_uc, department_code, nonGroc)]
fwrite(prod, "/home/mallick/Desktop/Nielsen/Data/CleanComScore/prod.csv")
