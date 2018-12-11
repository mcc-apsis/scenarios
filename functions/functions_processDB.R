#== Public databases ===============================

process_ar5data <- function(i_path_data, REMOVE_NAs=TRUE, REMOVE_FRAGPOL=FALSE, VERBOSE=TRUE, DEBUG=FALSE) {
  
  #== INITIALISE ===========================================
  if(VERBOSE) cat(paste0("process_ar5data: INITIALISE\n"))
  source("functions/ar5clean.R", encoding = 'UTF-8')
  source("functions/ar5join.R", encoding = 'UTF-8')
  source("functions/ar5rm_2050models.R", encoding = 'UTF-8')
  count_pathway <- function(i_dataBefore, i_dataAfter) {
    tmpBefore <- unique((i_dataBefore %>% mutate(model.scenario = paste0(model, ".", scenario)) %>% dplyr::select(model.scenario))$model.scenario)
    tmpAfter  <- unique((i_dataAfter  %>% mutate(model.scenario = paste0(model, ".", scenario)) %>% dplyr::select(model.scenario))$model.scenario)
    cat(paste("This process removed", length(tmpBefore[which(!tmpBefore %in% tmpAfter)]),"pathways.\n"))
    return()
  }
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_ar5data: READ IN DATA\n"))
  load(i_path_data)
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_ar5data: CLEAN DATA\n"))
  data_ar5 <- ar5data %>% 
    ar5rm_2050models() %>%                                  # Removing 2050 models
    ar5clean() %>%                                          # Clean data (e.g. remove NAs ...)
    ar5join()                                               # Join IAM results with scenario information
  
  if (DEBUG) count_pathway(ar5data, data_ar5)
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_ar5data: FILTER DATA\n"))
  data_ar5 <- data_ar5 %>% 
    select_variables()
  
  #== REMOVE NAs ===========================================
  if (REMOVE_NAs) {
    if(VERBOSE) cat(paste0("process_ar5data: REMOVE NAs\n"))
    tmp <- data_ar5
    # Merge model and scenario information
    data_ar5 <- data_ar5 %>% 
      unite(model.scenario, model, scenario, remove=FALSE) 
    # Remove NAs based on the variable Emissions|CO2 (Mandatory)
    tmp <- data_ar5 %>% 
      filter(!(is.na(value)) & period == 2050 & variable == "Emissions|CO2") %>%
      select(model, scenario) %>% 
      unite(model.scenario, model, scenario)
    data_ar5 <- data_ar5 %>% 
      filter(model.scenario %in% tmp$model.scenario) %>% 
      select(-model.scenario)
    
    if (DEBUG) count_pathway(tmp, data_ar5)
    
    rm(tmp)
  }
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  if(VERBOSE) cat(paste0("process_ar5data: CAREGORISE BY TEMPERATURE CATEGORY\n"))
  v_data <- data_ar5 %>% 
    left_join(categorise_tempGoals(i_path_data), by=c("model", "scenario")) %>% 
    mutate(tempcat = ifelse(climate_metric == "Forcing Kyoto Gases (MAGICC6)" & climate == "Category 1", "L2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Forcing Kyoto Gases (MAGICC6)" & climate == "Category 2", "M2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Forcing Kyoto Gases (MAGICC6)" & climate == "Category 3", "L3C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Forcing Kyoto Gases (MAGICC6)" & climate %in% c("Category 4","Category 5","Category 6","Category 7"), "Other", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2100" & climate == "Category 1", "L2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2100" & climate == "Category 2", "M2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2100" & climate == "Category 3", "L3C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2100" & climate %in% c("Category 4","Category 5","Category 6","Category 7"), "Other", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2050" & climate == "Category 1", "L2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2050" & climate == "Category 2", "M2C", tempcat)) %>% 
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2050" & climate == "Category 1 - 2", "M2C", tempcat)) %>%
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2050" & climate == "Category 2 - 4", "L3C", tempcat)) %>%
    mutate(tempcat = ifelse(climate_metric == "Carbon Budget 2050" & climate %in% c("Category 3 - 4", "Category 3 - 5", "Category 4 - 5", "Category 4 - 6","Category 5 - 6","Category 6","Category 7"), "Other", tempcat)) %>% 
    mutate(source = ifelse(grepl("ROSE", scenario),   "RoSE",
                           ifelse(grepl("AMPERE", scenario), "AMPERE",
                                  ifelse(grepl("LIMITS", scenario), "LIMITS",
                                         ifelse(grepl("EMF27", scenario),  "EMF27",
                                                ifelse(grepl("AME", scenario),    "AME", 
                                                       ifelse(grepl("GEA", scenario),    "GEA", "RCP-MIT")))))))
  
  if (DEBUG) count_pathway(ar5data, v_data)
  
  #== REMOVE FRAGMENTED POLICY SCENARIOs ===================
  if (REMOVE_FRAGPOL) {
    if(VERBOSE) cat(paste0("process_ar5data: REMOVE FRAG;EMTED POLICY SCENARIOS\n"))
    tmp <- v_data
    v_data <- v_data %>%
      filter(!policy %in% c("P4","P4+","P5"))
    
    if (DEBUG) count_pathway(tmp, v_data)
    
    rm(tmp)
  }
  
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  if(VERBOSE) cat(paste0("process_ar5data: CATEGORISE DATA BY POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  tmp <- v_data
  v_data <- v_data %>% 
    mutate(timingcat = "NA") %>% 
    mutate(timingcat = ifelse(policy %in% c("P7"),                "other", timingcat)) %>% 
    mutate(timingcat = ifelse(grepl("weak", tolower(scenario)),   "weakPol", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("strpol", tolower(scenario)), "strPol", timingcat)) %>%
    mutate(timingcat = ifelse(policy %in% c("P1", "P1+"),         "immediate", timingcat)) %>%
    mutate(timingcat = ifelse(policy == "P2",                     "delay2020", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P3", "P3+"),         "delay2030", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P4","P4+"),          "fragmented3050", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P5","P6"),           "fragmented3070", timingcat)) %>% 
    mutate(techcat = "other") %>% 
    mutate(techcat = ifelse(technology == "T0", "default", techcat)) %>% 
    mutate(techcat = ifelse(technology %in% c("T1", "T3") | grepl("nbecs", tolower(scenario)), "noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbio|limbe|conv", tolower(scenario)), "limBio", techcat)) %>%
    mutate(techcat = ifelse(grepl("lowei", tolower(scenario)), "lowEI", techcat)) %>% 
    mutate(techcat = ifelse(grepl("noccs|eere", tolower(scenario)), "noCCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limtech", tolower(scenario)), "limBio&noCCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("biomin", tolower(scenario)), "limBio", techcat)) %>%
    mutate(techcat = ifelse(grepl("limbe", tolower(scenario)) & grepl("nbecs", tolower(scenario)), "limBio&noBECCS", techcat)) 
  
  if (DEBUG) count_pathway(tmp, v_data)
  
  rm(tmp)
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_ar5data: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat()
  
  return(v_data)
}

process_ampere <- function(i_path_data, i_path_ar5data, REMOVE_FRAGPOL=FALSE, VERBOSE=TRUE, DEBUG=FALSE) {
  #== INITIALISE ===========================================
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_ampere: READ IN DATA\n"))
  load(i_path_data)
  load(i_path_ar5data)
  rm("ar5data")
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_ampere: FILTER DATA\n"))
  v_data <- data_ampere %>% 
    select_variables()
  
  #== REMOVE FRAGMENTED POLICY SCENARIOS ===================
  if (REMOVE_FRAGPOL) {
    if(VERBOSE) cat(paste0("process_ampere: REMOVE FRAGMENTED POLICY SCENARIOS\n"))
    v_data <- v_data %>% 
      dplyr::filter(grepl("AMPERE2", scenario))  # Select only non fragmented policy scenario
  }
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_ampere: CLEAN DATA\n"))
  v_data <- v_data %>% 
    mutate(model    = paste(model)) %>% 
    mutate(scenario = paste(scenario))
  
  #== HARMONISE MODEL NAMES WITH AR5 DB ====================
  if(VERBOSE) cat(paste0("process_ampere: HARMONISE MODEL NAMES WITH AR5 DB\n"))
  v_data <- v_data %>% 
    mutate(mod=model) %>% 
    mutate(model= ifelse(grepl("AMPERE", scenario) & mod == "DNE21", "DNE21 V.12",
                         ifelse(grepl("AMPERE", scenario) & mod == "GCAM", "GCAM 3.0",
                                ifelse(grepl("AMPERE", scenario) & mod == "GEM-E3", "GEM-E3-ICCS",
                                       ifelse(grepl("AMPERE", scenario) & mod == "IMACLIM", "IMACLIM v1.1",
                                              ifelse(grepl("AMPERE", scenario) & mod == "MERGE-ETL", "MERGE_EMF27",
                                                     ifelse(grepl("AMPERE", scenario) & mod == "MESSAGE", "MESSAGE V.4",
                                                            ifelse(grepl("AMPERE", scenario) & mod == "POLES", "POLES AMPERE",
                                                                   ifelse(grepl("AMPERE", scenario) & mod == "IMAGE", "IMAGE 2.4",
                                                                          ifelse(grepl("AMPERE", scenario) & mod == "REMIND", "REMIND 1.5",
                                                                                 ifelse(grepl("AMPERE", scenario) & mod == "WITCH", "WITCH_AMPERE", model))))))))))) 
  
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  if(VERBOSE) cat(paste0("process_ampere: CATEGORISE DATA BY TEMPERATURE CATEGORY\n"))
  #-- Add 2100 GMT (MAGICC6 median) ---
  v_data <- left_join(
    v_data,
    v_data %>% 
      filter(region=="World", period == 2100, variable == "Temperature|Global Mean|MAGICC6|MED") %>% 
      select(-variable, -region, -unit, -period) %>% 
      rename(temp2100 = value),
    by = c("mod", "scenario", "model"))
  
  #-- Join with exceedance probability for temperature targets and classify ---
  v_data <- v_data %>% 
    left_join(v_data %>% 
                filter(region == "World") %>% 
                filter(grepl("Temperature.*Exceedance Probability", variable)) %>% 
                select(model,scenario,variable,period,value) %>% 
                group_by(model,scenario,variable) %>% 
                summarize(
                  value2100 = max(ifelse(period == 2100, value, NA), na.rm=TRUE),
                  valueMax  = max(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                gather(type, value, -model, -scenario, -variable) %>% 
                mutate(variable.type=paste0(variable,".",type)) %>% 
                filter(variable.type %in% c("Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100",
                                            "Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax",
                                            "Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax")) %>% 
                select(-variable,-type) %>% 
                spread(variable.type, value) %>%
                mutate(tempcat = ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` <= 0.5, "1p5C", 
                                        ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` > 0.5 & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 1/3, "L2C", 
                                               ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` > 1/3  & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 0.5, "M2C", 
                                                      ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` > 0.5  & `Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax` <= 1/3, "L3C", "Other"))))) %>% 
                select(model,scenario,tempcat),
              by=c("model", "scenario"))
  
  
  #== JOIN WITH AR5 DATA CLASSIFICATION ===========
  if(VERBOSE) cat(paste0("process_ampere: JOIN WITH AR5 DATA CLASSIFICATION\n"))
  v_data <- left_join(
    v_data,
    ar5scen,
    by = c("model", "scenario"))
  
  #== REMOVE FRAGMENTED POLICY SCENARIOS ==========
  if (REMOVE_FRAGPOL) {
    if(VERBOSE) cat(paste0("process_ampere: REMOVE FRAGMENTED POLICY SCENARIOS\n"))
    v_data <- v_data %>%
      filter(!policy %in% c("P4","P4+","P5"))    
  }
  
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  if(VERBOSE) cat(paste0("process_ampere: CATEGORISE DATA BY POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  v_data <- v_data %>% 
    mutate(timingcat = "NA") %>% 
    mutate(timingcat = ifelse(policy %in% c("P0"), "baseline", timingcat)) %>%
    mutate(timingcat = ifelse(policy %in% c("P1", "P1+"), "immediate", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("weak", tolower(scenario)), "weakPol", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("strpol", tolower(scenario)), "strPol", timingcat)) %>%
    mutate(timingcat = ifelse(policy == "P2", "delay2020", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P3", "P3+"), "delay2030", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("OPT", scenario)     & !grepl("Base", scenario), "immediate", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("HST|LST", scenario) & !grepl("Base", scenario), "delay2030", timingcat)) %>% 
    mutate(techcat = "other") %>% 
    mutate(techcat = ifelse(technology == "T0", "default", techcat)) %>% 
    mutate(techcat = ifelse(technology %in% c("T1", "T3") | grepl("nbecs", tolower(scenario)), "noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbio|limbe|conv", tolower(scenario)), "limBio", techcat)) %>%
    mutate(techcat = ifelse(grepl("lowei", tolower(scenario)), "lowEI", techcat)) %>% 
    mutate(techcat = ifelse(grepl("noccs|eere", tolower(scenario)), "noCCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbe", tolower(scenario)) & grepl("nbecs", tolower(scenario)), "limBio&noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("FullTech", scenario),    "default", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("NucOff|Conv|LimSW", scenario), "other",   techcat))
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_ampere: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat() %>% 
    filter(!is.na(tempcat)) 
  
  return(v_data)
}

process_limits <- function(i_path_data, i_path_ar5data, REMOVE_FRAGPOL=FALSE, VERBOSE=TRUE, DEBUG=FALSE) {
  #== INITIALISE ===========================================
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_limits: READ IN DATA\n"))
  load(i_path_data)
  load(i_path_ar5data)
  rm("ar5data")
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_limits: FILTER DATA\n"))
  v_data <- data_limits %>% 
    select_variables()
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_limits: CLEAN DATA\n"))
  v_data <- v_data %>% 
    mutate(model    = paste(model)) %>% 
    mutate(scenario = paste(scenario))
  
  #== HARMONISE MODEL NAMES WITH AR5 DB ====================
  if(VERBOSE) cat(paste0("process_limits: HARMONISE MODEL NAMES WITH AR5 DB\n"))
  v_data <- v_data %>% 
    mutate(mod=model) %>% 
    mutate(model= ifelse(grepl("LIMITS", scenario) & mod == "GCAM", "GCAM 3.1",
                         ifelse(grepl("LIMITS", scenario) & mod == "AIM-Enduse", "AIM-Enduse[Backcast]",
                                ifelse(grepl("LIMITS", scenario) & mod == "TIAM-ECN", "TIAM-ECN",
                                       ifelse(grepl("LIMITS", scenario) & mod == "MESSAGE", "MESSAGE V.4",
                                              ifelse(grepl("LIMITS", scenario) & mod == "IMAGE", "IMAGE 2.4",
                                                     ifelse(grepl("LIMITS", scenario) & mod == "REMIND", "REMIND 1.5",
                                                            ifelse(grepl("LIMITS", scenario) & mod == "WITCH", "WITCH_LIMITS", model))))))))
    
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  if(VERBOSE) cat(paste0("process_limits: CATEGORISE DATA BY TEMPERATURE CATEGORY\n"))
  #-- Add 2100 GMT (MAGICC6 median) ---
  v_data <- left_join(
    v_data,
    v_data %>% 
      filter(region=="World", period == 2100, variable == "Temperature|Global Mean|MAGICC6|MED") %>% 
      select(-variable, -region, -unit, -period) %>% 
      rename(temp2100 = value),
    by = c("mod", "scenario", "model"))
  
  #-- Join with exceedance probability for temperature targets and classify ---
  v_data <- v_data %>% 
    left_join(v_data %>% 
                filter(region == "World") %>% 
                filter(grepl("Temperature.*Exceedance Probability", variable)) %>% 
                select(model,scenario,variable,period,value) %>% 
                group_by(model,scenario,variable) %>% 
                summarize(
                  value2100 = max(ifelse(period == 2100, value, NA), na.rm=TRUE),
                  valueMax  = max(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                gather(type, value, -model, -scenario, -variable) %>% 
                mutate(variable.type=paste0(variable,".",type)) %>% 
                filter(variable.type %in% c("Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100",
                                            "Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax",
                                            "Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax")) %>% 
                select(-variable,-type) %>% 
                spread(variable.type, value) %>%
                mutate(tempcat = ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` <= 0.5, "1p5C", 
                                        ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` > 0.5 & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 1/3, "L2C", 
                                               ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` > 1/3  & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 0.5, "M2C", 
                                                      ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` > 0.5  & `Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax` <= 1/3, "L3C", "Other"))))) %>% 
                select(model,scenario,tempcat),
              by=c("model", "scenario"))
  
  
  #== JOIN WITH AR5 DATA CLASSIFICATION ===========
  if(VERBOSE) cat(paste0("process_limits: JOIN WITG AR5 DATA CLASSIFICATION\n"))
  v_data <- left_join(
    v_data,
    ar5scen,
    by = c("model", "scenario"))
  
  #== REMOVE FRAGMENTED POLICY SCENARIOS ==========
  if (REMOVE_FRAGPOL) {
    if(VERBOSE) cat(paste0("process_limits: REMOVE FRAGMENTED POLICY SCENARIOS\n"))
    v_data <- v_data %>%
      filter(!policy %in% c("P4","P4+","P5"))    
  }
  
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  if(VERBOSE) cat(paste0("process_limits: CATEGORISE DATA BY POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  v_data <- v_data %>% 
    mutate(timingcat = "NA") %>% 
    mutate(timingcat = ifelse(policy %in% c("P0"), "baseline", timingcat)) %>%
    mutate(timingcat = ifelse(policy %in% c("P1", "P1+"), "immediate", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("weak", tolower(scenario)), "weakPol", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("strpol", tolower(scenario)), "strPol", timingcat)) %>%
    mutate(timingcat = ifelse(policy == "P2", "delay2020", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P3", "P3+"), "delay2030", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("OPT", scenario)     & !grepl("Base", scenario), "immediate", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("HST|LST", scenario) & !grepl("Base", scenario), "delay2030", timingcat)) %>% 
    mutate(techcat = "other") %>% 
    mutate(techcat = ifelse(technology == "T0", "default", techcat)) %>% 
    mutate(techcat = ifelse(technology %in% c("T1", "T3") | grepl("nbecs", tolower(scenario)), "noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbio|limbe|conv", tolower(scenario)), "limBio", techcat)) %>%
    mutate(techcat = ifelse(grepl("lowei", tolower(scenario)), "lowEI", techcat)) %>% 
    mutate(techcat = ifelse(grepl("noccs|eere", tolower(scenario)), "noCCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbe", tolower(scenario)) & grepl("nbecs", tolower(scenario)), "limBio&noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("FullTech", scenario),    "default", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("NucOff|Conv|LimSW", scenario), "other",   techcat))
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_limits: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat() %>% 
    filter(!is.na(tempcat)) 
  
  return(v_data)
}

process_rose <- function(i_path_data, i_path_ar5data, i_dataAR5, REMOVE_FRAGPOL=FALSE, VERBOSE=TRUE, DEBUG=FALSE) {
  #== INITIALISE ===========================================
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_rose: READ IN DATA\n"))
  load(i_path_data)
  load(i_path_ar5data)
  rm("ar5data")
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_rose: FILTER DATA\n"))
  v_data <- data_rose %>% 
    select_variables()
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_rose: CLEAN DATA\n"))
  v_data <- v_data %>% 
    mutate(model    = paste(model)) %>% 
    mutate(scenario = paste(scenario))
  
  #== HARMONISE MODEL NAMES WITH AR5 DB ====================
  if(VERBOSE) cat(paste0("process_rose: HARMONISE MODEL NAMES WITH AR5 DB\n"))
  v_data <- v_data %>% 
    mutate(mod=model) %>% 
    mutate(model= ifelse(grepl("ROSE", scenario)   & mod == "GCAM", "GCAM 3.0",
                         ifelse(grepl("ROSE", scenario)   & mod == "REMIND", "REMIND 1.4",
                                ifelse(grepl("ROSE", scenario)   & mod == "WITCH", "WITCH_ROSE", model))))
  
  
  #== HARMONISE SCENARIO NAMES WITH AR5 DB ====================
  if(VERBOSE) cat(paste0("process_rose: HARMONISE SCENARIO NAMES WITH AR5 DB\n"))
  v_data <- v_data %>% 
    mutate(scenario=
             #--Baseline scenarios ---
             ifelse(scenario == "ROSE111", "ROSE BAU DEF",
                    ifelse(scenario == "ROSE121", "ROSE BAU SL Gr",
                           ifelse(scenario == "ROSE131", "ROSE BAU FS Gr",
                                  ifelse(scenario == "ROSE141", "ROSE BAU HI Pop",
                                         ifelse(scenario == "ROSE1B1", "ROSE BAU SL Gr SL Con",
                                                ifelse(scenario == "ROSE151", "ROSE BAU FS Gr SL Con",
                                                       ifelse(scenario == "ROSE161", "ROSE BAU HI Fos",
                                                              ifelse(scenario == "ROSE171", "ROSE BAU LO Fos",
                                                                     ifelse(scenario == "ROSE181", "ROSE BAU LO Oil",
                                                                            ifelse(scenario == "ROSE191", "ROSE BAU HI Coal",
                                                                                   ifelse(scenario == "ROSE1A1", "ROSE BAU LO Oil HI Gas",
                                                                                          ifelse(scenario == "ROSE1C1", "ROSE BAU HI Gas",
                                                                                                 #-- 450ppm / Likely 2°C scenarios ---
                                                                                                 ifelse(scenario == "ROSE211", "ROSE 450 DEF",
                                                                                                        ifelse(scenario == "ROSE221", "ROSE 450 SL Gr",
                                                                                                               ifelse(scenario == "ROSE231", "ROSE 450 FS Gr",
                                                                                                                      ifelse(scenario == "ROSE241", "ROSE 450 HI Pop",
                                                                                                                             ifelse(scenario == "ROSE251", "ROSE 450 FS Gr SL Con",
                                                                                                                                    ifelse(scenario == "ROSE261", "ROSE 450 HI Fos",
                                                                                                                                           ifelse(scenario == "ROSE271", "ROSE 450 LO Fos",
                                                                                                                                                  ifelse(scenario == "ROSE281", "ROSE 450 LO Oil",
                                                                                                                                                         ifelse(scenario == "ROSE291", "ROSE 450 HI Coal",
                                                                                                                                                                ifelse(scenario == "ROSE2A1", "ROSE 450 LO Oil HI Gas",
                                                                                                                                                                       #-- 550ppm / Medium 2°C scenarios --
                                                                                                                                                                       ifelse(scenario == "ROSE311", "ROSE 550 DEF",
                                                                                                                                                                              ifelse(scenario == "ROSE321", "ROSE 550 SL Gr",
                                                                                                                                                                                     ifelse(scenario == "ROSE331", "ROSE 550 FS Gr",
                                                                                                                                                                                            ifelse(scenario == "ROSE341", "ROSE 550 HI Pop",
                                                                                                                                                                                                   ifelse(scenario == "ROSE351", "ROSE 550 FS Gr SL Con",
                                                                                                                                                                                                          ifelse(scenario == "ROSE361", "ROSE 550 HI Fos",
                                                                                                                                                                                                                 ifelse(scenario == "ROSE371", "ROSE 550 LO Fos",
                                                                                                                                                                                                                        ifelse(scenario == "ROSE381", "ROSE 550 LO Oil",
                                                                                                                                                                                                                               ifelse(scenario == "ROSE391", "ROSE 550 HI Coal",
                                                                                                                                                                                                                                      ifelse(scenario == "ROSE3A1", "ROSE 550 LO Oil HI Gas",
                                                                                                                                                                                                                                             #-- Weak policy scenarios --
                                                                                                                                                                                                                                             ifelse(scenario == "ROSE411", "ROSE WEAK-POL DEF",
                                                                                                                                                                                                                                                    ifelse(scenario == "ROSE421", "ROSE WEAK-POL SL Gr",
                                                                                                                                                                                                                                                           ifelse(scenario == "ROSE431", "ROSE WEAK-POL FS Gr",
                                                                                                                                                                                                                                                                  ifelse(scenario == "ROSE451", "ROSE WEAK-POL FS Gr SL Con",
                                                                                                                                                                                                                                                                         ifelse(scenario == "ROSE511", "ROSE WEAK-2020 DEF",
                                                                                                                                                                                                                                                                                ifelse(scenario == "ROSE521", "ROSE WEAK-2020 SL Gr",
                                                                                                                                                                                                                                                                                       ifelse(scenario == "ROSE531", "ROSE WEAK-2020 FS Gr",
                                                                                                                                                                                                                                                                                              ifelse(scenario == "ROSE551", "ROSE WEAK-2020 FS Gr SL Con",
                                                                                                                                                                                                                                                                                                     ifelse(scenario == "ROSE611", "ROSE WEAK-2030 DEF",
                                                                                                                                                                                                                                                                                                            ifelse(scenario == "ROSE621", "ROSE WEAK-2030 SL Gr",
                                                                                                                                                                                                                                                                                                                   ifelse(scenario == "ROSE631", "ROSE WEAK-2030 FS Gr",
                                                                                                                                                                                                                                                                                                                          ifelse(scenario == "ROSE651", "ROSE WEAK-2030 FS Gr SL Con", scenario)))))))))))))))))))))))))))))))))))))))))))))
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  if(VERBOSE) cat(paste0("process_rose: CATEGORISE DATA BY TEMPERATURE CATEGORY\n"))
  #-- Add 2100 GMT (MAGICC6 median) ---
  v_data <- left_join(
    v_data,
    v_data %>% 
      filter(region=="World", period == 2100, variable == "Temperature|Global Mean|MAGICC6|MED") %>% 
      select(-variable, -region, -unit, -period) %>% 
      rename(temp2100 = value),
    by = c("mod", "scenario", "model"))
  
  #-- Join with exceedance probability for temperature targets and classify ---
  v_data <- v_data %>% 
    left_join(v_data %>% 
                filter(grepl("ROSE", scenario)) %>% 
                inner_join(
                  i_dataAR5 %>%
                    filter(region == "World", period == 2100, variable == "Emissions|CO2") %>% 
                    select(model, scenario, tempcat),
                  by = c("model", "scenario")))
  
  
  #== JOIN WITH AR5 DATA CLASSIFICATION ===========
  if(VERBOSE) cat(paste0("process_rose: JOIN WITH AR5 DATA CLASSIFICATION\n"))
  v_data <- left_join(
    v_data,
    ar5scen,
    by = c("model", "scenario"))
  
  #== REMOVE FRAGMENTED POLICY SCENARIOS ==========
  if (REMOVE_FRAGPOL) {
    if(VERBOSE) cat(paste0("process_rose: REMOVE FRAGMENTED POLICY SCENARIO\n"))
    v_data <- v_data %>%
      filter(!policy %in% c("P4","P4+","P5"))    
  }
  
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  if(VERBOSE) cat(paste0("process_rose: CATEGORISE DATA BY POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  v_data <- v_data %>% 
    mutate(timingcat = "NA") %>% 
    mutate(timingcat = ifelse(policy %in% c("P0"), "baseline", timingcat)) %>%
    mutate(timingcat = ifelse(policy %in% c("P1", "P1+"), "immediate", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("weak", tolower(scenario)), "weakPol", timingcat)) %>%
    mutate(timingcat = ifelse(grepl("strpol", tolower(scenario)), "strPol", timingcat)) %>%
    mutate(timingcat = ifelse(policy == "P2", "delay2020", timingcat)) %>% 
    mutate(timingcat = ifelse(policy %in% c("P3", "P3+"), "delay2030", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("OPT", scenario)     & !grepl("Base", scenario), "immediate", timingcat)) %>% 
    mutate(timingcat = ifelse(model == "IMACLIM v1.1" & grepl("HST|LST", scenario) & !grepl("Base", scenario), "delay2030", timingcat)) %>% 
    mutate(techcat = "other") %>% 
    mutate(techcat = ifelse(technology == "T0", "default", techcat)) %>% 
    mutate(techcat = ifelse(technology %in% c("T1", "T3") | grepl("nbecs", tolower(scenario)), "noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbio|limbe|conv", tolower(scenario)), "limBio", techcat)) %>%
    mutate(techcat = ifelse(grepl("lowei", tolower(scenario)), "lowEI", techcat)) %>% 
    mutate(techcat = ifelse(grepl("noccs|eere", tolower(scenario)), "noCCS", techcat)) %>% 
    mutate(techcat = ifelse(grepl("limbe", tolower(scenario)) & grepl("nbecs", tolower(scenario)), "limBio&noBECCS", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("FullTech", scenario),    "default", techcat)) %>% 
    mutate(techcat = ifelse(model == "IMACLIM v1.1" & grepl("NucOff|Conv|LimSW", scenario), "other",   techcat))
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_rose: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat() %>% 
    filter(!is.na(tempcat)) 
  
  return(v_data)
}

process_sr15data <- function(i_path_data, VERBOSE=TRUE, DEBUG=FALSE) {
  
  #== INITIALISE ===========================================
  count_pathway <- function(i_dataBefore, i_dataAfter) {
    tmpBefore <- unique((i_dataBefore %>% mutate(model.scenario = paste0(model, ".", scenario)) %>% dplyr::select(model.scenario))$model.scenario)
    tmpAfter  <- unique((i_dataAfter  %>% mutate(model.scenario = paste0(model, ".", scenario)) %>% dplyr::select(model.scenario))$model.scenario)
    cat(paste("This process removed", length(tmpBefore[which(!tmpBefore %in% tmpAfter)]),"pathways.\n"))
    return()
  }
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_sr15: READ IN DATA\n"))
  load(i_path_data)
  
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_sr15: FILTER DATA\n"))
  v_data <- data_iamc15 %>% 
    select_variables()
  
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY (tempcat),
  # POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS 
  # (techcat) ==============
  if(VERBOSE) cat(paste0("process_sr15: CATEGORISE DATA BY TEMPERATURE CATEGORY, POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  v_data <- v_data %>% 
    left_join(data_iamc15 %>% 
                filter(region == "World") %>% 
                filter(grepl("Temperature.*Exceedance Probability.*MAGICC6", variable)) %>% 
                select(model,scenario,variable,period,value) %>% 
                group_by(model,scenario,variable) %>% 
                summarize(
                  value2100 = max(ifelse(period == 2100, value, NA), na.rm=TRUE),
                  valueMax  = max(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                gather(type, value, -model, -scenario, -variable) %>% 
                mutate(variable.type=paste0(variable,".",type)) %>% 
                filter(variable.type %in% c("AR5 climate diagnostics|Temperature|Exceedance Probability|1.5 °C|MAGICC6.value2100",
                                            "AR5 climate diagnostics|Temperature|Exceedance Probability|2.0 °C|MAGICC6.valueMax",
                                            "AR5 climate diagnostics|Temperature|Exceedance Probability|3.0 °C|MAGICC6.valueMax")) %>% 
                select(-variable,-type) %>% 
                spread(variable.type, value) %>%
                mutate(tempcat = ifelse(`AR5 climate diagnostics|Temperature|Exceedance Probability|1.5 °C|MAGICC6.value2100` <= 0.5, "1p5C", 
                                        ifelse(`AR5 climate diagnostics|Temperature|Exceedance Probability|1.5 °C|MAGICC6.value2100` > 0.5 & `AR5 climate diagnostics|Temperature|Exceedance Probability|2.0 °C|MAGICC6.valueMax` <= 1/3, "L2C", 
                                               ifelse(`AR5 climate diagnostics|Temperature|Exceedance Probability|2.0 °C|MAGICC6.valueMax` > 1/3  & `AR5 climate diagnostics|Temperature|Exceedance Probability|2.0 °C|MAGICC6.valueMax` <= 0.5, "M2C", 
                                                      ifelse(`AR5 climate diagnostics|Temperature|Exceedance Probability|2.0 °C|MAGICC6.valueMax` > 0.5  & `AR5 climate diagnostics|Temperature|Exceedance Probability|3.0 °C|MAGICC6.valueMax` <= 1/3, "L3C", "Other"))))) %>% 
                mutate(timingcat = ifelse(grepl("ADVANCE_2020|Delay2020|NPi2020", scenario), "delay2020", 
                                          ifelse(grepl("ADVANCE_2030", scenario), "delay2030", 
                                                 ifelse(grepl("INDC", scenario), "INDC", 
                                                        ifelse(grepl("NoPolicy|Reference|Baseline|BAU|base", scenario), "baseline", "immediate"))))) %>% 
                mutate(techcat = ifelse(grepl("nobeccs|noCDR", scenario), "noBECCS", 
                                        ifelse(grepl("limbio", scenario), "limBio", 
                                               ifelse(grepl("lowEI", scenario), "lowEI", "Other")))) %>% 
                select(model,scenario,tempcat, timingcat, techcat),
              by=c("model", "scenario"))
  
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_sr15: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat()
  
  return(v_data)
}


#== Non-public datasets ===========================

process_luderer2013 <- function(i_path_data, VERBOSE=TRUE, DEBUG=FALSE) {
  #== INITIALISE ===========================================
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_luderer2013: READ IN DATA\n"))
  load(i_path_data)
  tryCatch(
    load(file.path(dirname(i_path_data), "one_five_scenarios.Rdata")),
    error = function(e) {
      cat(paste0("Error: The file one_five_scenarios.Rdata could not be found. Make sure it is located in the same directory as one_long_scenarios.RData.\n"))
      stop()
    }
  )
  tryCatch(
    data_1p5v2scenclass <- readxl::read_xls(file.path(dirname(i_path_data), "1p5CSCENs_categorization_forJanMINX.xls")),
    error = function(e) {
      cat(paste0("Error: The file one_five_scenarios.Rdata could not be found. Make sure it is located in the same directory as one_long_scenarios.RData.\n"))
      stop()
    }
  )
  
  
  #== FILTER DATA ==========================================
  if(VERBOSE) cat(paste0("process_luderer2013: FILTER DATA\n"))
  v_data <- one_five_long_all %>% 
    rename(period=year) %>% 
    select_variables() %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value))
  
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_luderer2013: CLEAN DATA\n"))
  #-- Change format of period and value columns to numeric
  v_data <- v_data %>% 
    mutate(value = as.numeric(value))%>% 
    mutate(period = as.numeric(period)) 
  
  #== JOIN WITH SCENARIO INFORMATION =======================
  if(VERBOSE) cat(paste0("process_luderer2013: JOIN WITH SCENARIO INFORMATION\n"))
  v_data <- left_join(
    v_data %>% 
      mutate(scenario = gsub("FFrun", "Scen", scenario)),
    one_five_scenarios,
    by=c("scenario"))
  
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  if(VERBOSE) cat(paste0("process_luderer2013: CATEGORISE DATA BY TEMPERATURE CATEGORY\n"))
  #-- JOIN WITH JOERI'S TEMPERATURE CATEGORIES --
  v_data <- v_data %>% 
    mutate(scenIIASA = gsub("Scen", "REMIND_Scen", scenario)) %>%
    left_join(data_1p5v2scenclass[-1,c(1,2)],
              by=c("scenIIASA"="IIASADB name")) %>% 
    mutate(tempcat = ifelse(X__1 == "A", "1.5°C scenario", 
                            ifelse(X__1 == "B", "Likely 2.0°C scenario", "Medium 2.0°C scenario"))) %>% 
    select(-`X__1`)
  
  #-- ADD OTHER SCENARIO CATEGORIES ----------------
  v_data <- v_data %>% 
    mutate(tempcatREMIND = ifelse(temp_cat == "1.5C", "1p5C",
                                  ifelse(temp_cat == "2C" & `2°c in 2100` >= 2/3, "L2C",
                                         ifelse(temp_cat == "2C" & `2°c in 2100` < 2/3, "M2C",
                                                ifelse(temp_cat == "2C" & tempcat == "L2C", "L2C",
                                                       ifelse(temp_cat == "greater_than_2" & `2100 temp(median)` <= 3.0, "M3C", "Other"))))))
  
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  if(VERBOSE) cat(paste0("process_luderer2013: CATEGORISE DATA BY POLICY TIMING AND TECHNOLOGICAL OPTIONS\n"))
  v_data <- v_data %>% 
    mutate(timingcat = "NA") %>% 
    mutate(timingcat = ifelse(timing == "Immediate", "immediate", timingcat)) %>%
    mutate(timingcat = ifelse(timing == "WeakPol",   "weakPol",   timingcat)) %>%
    mutate(timingcat = ifelse(timing == "Frag2015",  "delay2015", timingcat)) %>% 
    mutate(timingcat = ifelse(timing == "Frag2020",  "delay2020", timingcat)) %>% 
    mutate(timingcat = ifelse(timing == "Frag2030",  "delay2030", timingcat)) %>% 
    mutate(techcat = "other") %>% 
    mutate(techcat = ifelse(technology == "Default", "default", techcat)) %>%
    mutate(techcat = ifelse(technology == "NoCCS",   "noCCS",   techcat)) %>%
    mutate(techcat = ifelse(technology == "NoBECCS", "noBECCS", techcat)) %>%
    mutate(techcat = ifelse(technology == "LowEI",   "lowEI",   techcat)) %>%
    mutate(techcat = ifelse(technology == "LimBio",  "limBio",  techcat))
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_luderer2013: NORMALISE DATA\n"))
  v_data <- v_data %>% 
    normaliseDataFrameFormat()
  
  return(v_data)
}

process_rogelj2015 <- function(i_path_data, VERBOSE=TRUE, DEBUG=FALSE) {
  #== INITIALISE ===========================================
  
  #== READ IN DATA =========================================
  if(VERBOSE) cat(paste0("process_rogelj2015: READ IN DATA\n"))
  data_1p5v2          <- read.delim(i_path_data, sep = "\t") %>% 
    mutate(model = "MESSAGE")
  names(data_1p5v2) <- c("scenario", "region", "variable", "unit", paste(seq(2010,2100,10)), "model")
  data_1p5v2 <- data_1p5v2 %>% 
    tidyr::gather(period, value, -model, -scenario, -region, -variable, -unit) %>% 
    dplyr::mutate(period = as.numeric(period)) %>% 
    dplyr::select(model, scenario, region, variable, unit, period, value)
  tryCatch(
    data_1p5v2scenclass <- readxl::read_xls(file.path(dirname(i_path_data), "1p5CSCENs_categorization_forJanMINX.xls")),
    error = function(e) {
      cat(paste0("Error: The file one_five_scenarios.Rdata could not be found. Make sure it is located in the same directory as one_long_scenarios.RData.\n"))
      stop()
    }
  )
  
  #== CLEAN DATA ===========================================
  if(VERBOSE) cat(paste0("process_rogelj2015: CLEAN DATA\n"))
  v_data <- data_1p5v2 %>%
    dplyr::mutate(variable = paste(variable)) %>%
    dplyr::mutate(variable = ifelse(variable == "Final Energy|Total",  "Final Energy", variable)) %>%
    dplyr::mutate(variable = ifelse(variable == "Emissions|CO2|Total", "Emissions|CO2", variable)) %>%
    dplyr::mutate(variable = ifelse(variable == "Emissions|CO2|Carbon Capture and Storage|Total", "Emissions|CO2|Carbon Capture and Storage", variable)) %>%
    rbind(data_1p5 %>%
            dplyr::filter(region == "World") %>%
            #dplyr::filter(filter == "feasible") %>%
            dplyr::mutate(variable = paste(variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "FE",                                          "Final Energy", variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "Emi|CO2",                                     "Emissions|CO2", variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "Emi|CO2|Carbon Capture and Storage",          "Emissions|CO2|Carbon Capture and Storage", variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "Emi|CO2|Carbon Capture and Storage|Biomass",  "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "Emi|CO2|Land-Use Change",                     "Emissions|CO2|Land-Use Change", variable)) %>%
            dplyr::mutate(variable = ifelse(variable == "Emi|CO2|Fossil fuels and Industry",           "Emissions|CO2|Fossil Fuels and Industry", variable)) %>%
            dplyr::filter(variable %in% c("Emissions|CO2|Carbon Capture and Storage|Biomass", "Emissions|CO2|Carbon Capture and Storage",
                                          "Emissions|CO2|Fossil Fuels and Industry",
                                          "Emissions|CO2|Land-Use Change",                    "Emissions|CO2",
                                          "Final Energy",                                     "Price|Carbon")) %>%
            dplyr::mutate(scenario = paste(scenario)) %>%
            dplyr::filter(grepl("FFrun", scenario)) %>%
            dplyr::mutate(scenario = gsub("FFrun", "REMIND_Scen", scenario)) %>%
            dplyr::filter(scenario %in% data_1p5v2scenclass$`IIASADB name`)) %>%
    dplyr::inner_join(data_1p5v2scenclass[-1,c(1,2)],
                      by=c("scenario"="IIASADB name")) %>%
    dplyr::mutate(tempcat = ifelse(X__1 == "A", "1p5C",
                                   ifelse(X__1 == "B", "L2C", "M2C"))) %>%
    dplyr::select(-`X__1`)
  
  #== FILTER DATA ==========================================
  
  #== CATEGORISE DATA BY TEMPERATURE CATEGORY ==============
  
  #== CATEGORISE DATA BY POLICY TIMING (timingcat) AND TECHNOLOGICAL OPTIONS (techcat) ==============
  
  #== NORMALISE DATA ============================
  if(VERBOSE) cat(paste0("process_rogelj2015: NORMALISE DATA\n"))
  v_data <- v_data %>%
    normaliseDataFrameFormat()
  
  return(v_data)
}


#== Other datasets ================================
process_gcp <- function(i_path_data, VERBOSE=TRUE, DEBUG=FALSE) {
  out <- readxl::read_xlsx(i_path_data, sheet = 2, range = "A22:C79") %>%
    rename(period=Year) %>% 
    rename(`Emissions|CO2|Fossil Fuels and Industry` = `fossil fuel and cement emissions`) %>% 
    rename(`Emissions|CO2|Land Use` = `land-use change emissions`) %>% 
    mutate(`Emissions|CO2` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Land Use`) %>% 
    gather(variable, value, -period) %>% 
    mutate(model = "Global Carbon Project (2015)") %>% 
    mutate(scenario = "Historical") %>% 
    mutate(unit = "Mt(CO2)/yr") %>% 
    mutate(value = value*44/12*1000) %>% 
    mutate(region = "World") %>% 
    select(model,scenario,region,variable,unit,period,value)
  
  return(out)
}

#== Other functions ===============================

# Creates a new variable with the temperature goal categories: 1p5C, L2C, M2C, L3C, Other
categorise_tempGoals <- function(i_path_data, dataset="AR5") {
  
  out <- data.frame()
  
  if (dataset == "AR5") {
    load(i_path_data)
    
    out <- ar5data %>% 
      filter(grepl("Temperature.*Exceedance Probability", variable)) %>% 
      select(model,scenario,variable,period,value) %>% 
      group_by(model,scenario,variable) %>% 
      summarize(
        value2100 = max(ifelse(period == 2100, value, NA), na.rm=TRUE),
        valueMax  = max(value, na.rm=TRUE)) %>% 
      ungroup() %>% 
      gather(type, value, -model, -scenario, -variable) %>% 
      mutate(variable.type=paste0(variable,".",type)) %>% 
      filter(variable.type %in% c("Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100",
                                  "Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax",
                                  "Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax")) %>% 
      select(-variable,-type) %>% 
      spread(variable.type, value) %>%
      mutate(tempcat = ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` <= 0.5, "1p5C", 
                       ifelse(`Temperature|Exceedance Probability|1.5 degC|MAGICC6.value2100` > 0.5 & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 1/3, "L2C", 
                       ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax`  > 1/3 & `Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax` <= 0.5, "M2C", 
                       ifelse(`Temperature|Exceedance Probability|2.0 degC|MAGICC6.valueMax`  > 0.5 & `Temperature|Exceedance Probability|3.0 degC|MAGICC6.valueMax` <= 1/3, "L3C", "Other"))))) %>% 
      select(model,scenario,tempcat)
  }
  
  
  return(out)
  
}


normaliseDataFrameFormat <- function(i_df) {
  res <- i_df %>% 
    mutate(model    = factor(model)) %>% 
    mutate(scenario = factor(scenario)) %>% 
    mutate(variable = factor(variable)) %>% 
    mutate(period   = as.numeric(period)) %>% 
    mutate(value    = as.numeric(value))
  
  return(res)
}



