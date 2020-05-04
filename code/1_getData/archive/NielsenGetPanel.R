# This gets the Nielsen panel data
library(data.table)
library(purrr)
library(stringr)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"

getPanel <- function(yr) {
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"),
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor",
                            "Household_Income", "Household_Size",
                            "Male_Head_Birth", "Female_Head_Birth",
                            "Female_Head_Education", "Male_Head_Education",
                            "Male_Head_Occupation", "Female_Head_Occupation",
                            "Marital_Status", "Race", "Hispanic_Origin",
                            "Panelist_ZipCd", "Scantrack_Market_Identifier_Desc",
                            "Fips_State_Cd", "Fips_State_Desc", "Fips_County_Cd"))
  setnames(panel, tolower(names(panel)))
  setnames(panel, c("household_cd", "scantrack_market_identifier_desc", "panelist_zipcd"),
           c("household_code", "market", "zip_code"))
  return(panel)
}
panel <- rbindlist(map(yr, getPanel))

# Adding income factors
panel[, "household_income" := cut(household_income, c(0, 13, 19, 26, 30),
                                  labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                  ordered_result = TRUE)]

# Adding household size factors
panel[, "household_size" := cut(household_size, c(0, 1, 2, 4, 9),
                                labels = c("1", "2", "3-4", "5+"),
                                ordered_result = TRUE)]

# Adding age factors
panel[, "age" := panel_year - (female_head_birth + male_head_birth) / 2]
panel[is.na(age), "age" := as.numeric(panel_year - female_head_birth)]
panel[is.na(age), "age" := as.numeric(panel_year - male_head_birth)]
panel[, "age" := cut(age, c(0, 24, 54, 150),
                     labels = c("<35", "35-54", "55+"),
                     ordered_result = TRUE)]

# Adding college indicator if at least 1 HoH has graduated college
panel[, "college" := 0]
panel[female_head_education >= 5 | male_head_education >= 5, "college" := 1]

# Adding Fips code
panel[, c("fips", "fips_state_cd", "fips_county_cd") :=
        .(as.integer(paste0(str_pad(fips_state_cd, 2, "left", "0"),
                            str_pad(fips_county_cd, 3, "left", "0"))), NULL, NULL)]

# Housekeeping
panel[, c("male_head_birth", "female_head_birth", "female_head_education", "male_head_education") := NULL]
fwrite(panel, "/home/mallick/Desktop/Nielsen/Data/CleanComScore/fullPanel.csv")
