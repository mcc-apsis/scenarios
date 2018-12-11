#==== OPTIONS =======================================
u_processDB <- TRUE
u_debug     <- FALSE
u_verbose   <- TRUE


#==== FILES AND PATHS ===============================
# Processed data (file to read to generate plots)
u_path_processedData <- "data/dataAll_20181203.RData"

# Public databases
u_path_ar5data    <- "data/ar5data.RData"
u_path_amperedata <- "data/AmperePublic_WP2+3_2014-10-09.RData"
u_path_limitsdata <- "data/LIMITSPUBLIC_2014-10-13.RData"
u_path_rosedata   <- "data/Final_RoSE_data.RData"
u_path_sr15data   <- "data/iamc15_scenario_data_all_regions_r1.RData"

# Non-public databases (ask owners before use)
u_path_luderer13  <- "data/one_five_long_all.Rdata"
u_path_rogelj15   <- "data/Export_data_JanMINX_20170804.txt" # Contains only limited data (used for NETs review)

# other datasets
u_path_rcpdata    <- "data/RCP.RData"
u_path_gcpdata    <- "data/Global_Carbon_Budget_2016v1.0.xlsx"


#==== DEFINE FUNCTION FOR VARIABLE SELECTION ========
# One can define its own function and place it inside the container of the select_variables function
# e.g. 
# source("functions/my_variable_selection.R")
# select_variables <- function(i_data) {
#   return(my_variable_selection(i_data))
# }
select_variables <- function(i_data) {
  out <- i_data %>% 
    filter(
      grepl("Temperature.*Exceedance Probability", variable) |
      variable %in% c(
        "Temperature|Global Mean|MAGICC6|MED",
        "Emissions|CO2"))
  return(out)
}