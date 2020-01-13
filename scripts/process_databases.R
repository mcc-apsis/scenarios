#==============================================================================================
# This routine generate a comprehensive set of scenarios by merging the following databases:
#   AMPERE, LIMITS, RoSE, SSP, AR5, Luderer et al (2013), Roeglj et al 2015 (1.5°C), SR1.5
#
# The function select_variables must be defined before running this script 
# (select_variables is in scripts/user_sectio.R).
#
# Pathways are categorised by:
#  - tempcat: 1p5C (1.5°C), L2C (Likely 2.0°C scenarios), M2C (Medium 2.0°C scenarios), L3C (Likely 3.0°C scenarios), Other (Other scenarios)
#    Note: All 1.5°C scenarios have a 50% likelihood of exceeding 1.5°C by 2100
#  - techcat: noCCS, no BECCS, limBio, lowEI, default, other
#  - timingcat: immediate, weakPol, strPol, delay2015, delay2020, delay2030
#  - allcat: optimal, limBio, lowEI, delay
#==============================================================================================

#==== INITIALISE ===============================


#==== PROCESS DATABASES ========================
#---- AR5 data ---------------------------------
v_dataAR5 <- process_ar5data(u_path_ar5data, REMOVE_NAs=TRUE, REMOVE_FRAGPOL=FALSE, VERBOSE=u_verbose, DEBUG=u_debug)
save(v_dataAR5, file="temp/v_data_AR5.RData")

#---- AMPERE, LIMITS and RoSE data -------------
v_dataALR <- rbind(
  process_ampere(u_path_amperedata, u_path_ar5data, VERBOSE=u_verbose, DEBUG=u_debug),
  process_limits(u_path_limitsdata, u_path_ar5data, VERBOSE=u_verbose, DEBUG=u_debug),
  process_rose(u_path_rosedata, u_path_ar5data, v_dataAR5, VERBOSE=u_verbose, DEBUG=u_debug))
save(v_dataALR, file="temp/v_data_AMPERELIMITSROSE.RData")

#---- IPCC SR15 2018 data ---------------------
v_dataSR15 <- process_sr15data(u_path_sr15data, VERBOSE=u_verbose, DEBUG=u_debug)
save(v_dataSR15, file="temp/v_dataSR15.RData")

if (u_nonPublic) {
  #---- Luderer 2013 data ------------------------
  v_dataL13 <- process_luderer2013(u_path_luderer13, VERBOSE=u_verbose, DEBUG=u_debug)
  save(v_dataL13, file="temp/v_data_Luderer13.RData")
  
  # #---- Rogelj 2015 data ------------------------
  # v_dataR15 <- process_rogelj2015(u_path_rogelj15, VERBOSE=u_verbose, DEBUG=u_debug)
  # save(v_dataR15, file="temp/v_dataRogelj15.RData")
} else {
  v_dataL13 <- data.frame()
}


#==== MERGE DATA ========================
#-- Data categorised by temperature targets only -----------
if (u_nonPublic){
combined_data <- rbind(
  v_dataAR5 %>% 
    filter(!grepl("AMPERE|LIMITS|ROSE", scenario)) %>%             # Removing AMPERE, LIMITS and ROSE scenarios
    select(model,scenario,region,variable,period,value,tempcat),
  # AMPERE, LIMITS, RoSE
  v_dataALR %>% 
    select(model,scenario,region,variable,period,value,tempcat),
  #Luderer et al 2013
  v_dataL13 %>% 
    select(model,scenario,region,variable,period,value,tempcat),
  # SR15 db
  v_dataSR15 %>% 
    filter(!grepl("EMC", scenario)) %>%                            # Removing EMC scenarios that come from Luderer et al 2013
    select(model,scenario,region,variable,period,value,tempcat))
} else {
combined_data <- rbind(
  v_dataAR5 %>% 
    filter(!grepl("AMPERE|LIMITS|ROSE", scenario)) %>%             # Removing AMPERE, LIMITS and ROSE scenarios
    select(model,scenario,region,variable,period,value,tempcat),
  # AMPERE, LIMITS, RoSE
  v_dataALR %>% 
    select(model,scenario,region,variable,period,value,tempcat),
  # SR15 db
  v_dataSR15 %>% 
    filter(!grepl("EMC", scenario)) %>%                            # Removing EMC scenarios that come from Luderer et al 2013
    select(model,scenario,region,variable,period,value,tempcat))
}

v_data_tempTargets <- combined_data %>%
  mutate(model    = factor(model)) %>% 
  mutate(scenario = factor(scenario)) %>% 
  mutate(period   = as.numeric(period)) %>% 
  filter(period %in% c(2005, seq(2010, 2100, 10))) %>% 
  mutate(tempcat   = factor(tempcat, levels=c("1p5C", "L2C", "M2C", "L3C", "Other"), ordered=TRUE)) %>% 
  mutate(value = as.numeric(value))

#-- Split between world and regional data -------------
v_data_tempTargets_world <- v_data_tempTargets %>% 
  filter(region == "World")
v_data_tempTargets_regional <- v_data_tempTargets %>% 
  filter(region != "World")

#--Data categorised by temperature targets and scenario types (limBio, delayed, lowEI ...) ------
if (u_nonPublic){
  combined_data <- rbind(
    v_dataAR5 %>% 
      filter(!grepl("AMPERE|LIMITS|ROSE", scenario)) %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat),
    # AMPERE-LIMITS-RoSE
    v_dataALR %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat),
    # Luderer et al 2013
    v_dataL13 %>% 
      filter(tempcat != "") %>% 
      filter(filter == "feasible") %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat),
    # SR15 db
    v_dataSR15 %>% 
      filter(!grepl("EMC", scenario)) %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat))
} else {
  combined_data <- rbind(
    v_dataAR5 %>% 
      filter(!grepl("AMPERE|LIMITS|ROSE", scenario)) %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat),
    # AMPERE-LIMITS-RoSE
    v_dataALR %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat),
    # SR15 db
    v_dataSR15 %>% 
      filter(!grepl("EMC", scenario)) %>% 
      select(model,scenario,region,variable,period,value,tempcat,timingcat,techcat))
}

v_data_timeTechTemp <- combined_data %>%
  mutate(model    = factor(model)) %>% 
  mutate(scenario = factor(scenario)) %>% 
  mutate(period   = as.numeric(period)) %>% 
  filter(period %in% c(2005, seq(2010, 2100, 10))) %>% 
  mutate(tempcat   = factor(tempcat, levels=c("1p5C", "L2C", "M2C", "L3C", "Other"), ordered=TRUE)) %>% 
  mutate(timingcat = factor(timingcat, levels=c("NA", "baseline", "immediate", "weakPol", "strPol", "delay2015", "delay2020", "delay2030"), ordered=TRUE)) %>% 
  mutate(techcat   = factor(techcat, levels=c("default", "noCCS", "noBECCS", "limBio", "lowEI", "other"), ordered=TRUE)) %>% 
  unite(allcat, timingcat, techcat, sep=".", remove=FALSE) %>% 
  mutate(allcat    = factor(allcat, levels=c("NA.default", 
                                             "baseline.default",  "baseline.noCCS", "baseline.noBECCS", "baseline.limBio", "baseline.lowEI", "baseline.other",
                                             "immediate.default", "immediate.noCCS", "immediate.noBECCS", "immediate.limBio", "immediate.lowEI", "immediate.other", "immediate.NA",
                                             "weakPol.default", 
                                             "strPol.default", "strPol.noCCS", "strPol.noBECCS", "strPol.limBio", "strPol.lowEI",
                                             "delay2015.noCCS",                      "delay2015.limBio", "delay2015.lowEI", "delay2015.other", 
                                             "delay2020.default", "delay2020.noCCS", "delay2020.noBECCS", "delay2020.limBio", "delay2020.lowEI", "delay2020.other",
                                             "delay2030.default", "delay2030.noCCS", "delay2030.noBECCS", "delay2030.limBio", "delay2030.lowEI", "delay2030.other"), ordered=TRUE)) %>% 
  filter(!grepl("NA|weakPol|strPol|delay2015|delay2020", timingcat)) %>% 
  filter(!is.na(techcat)) %>% 
  mutate(allcat=paste(allcat)) %>% 
  mutate(allcat=ifelse(allcat == "immediate.default", "optimal", allcat)) %>% 
  mutate(allcat=ifelse(allcat == "immediate.noCCS", "noCCS", allcat)) %>% 
  mutate(allcat=ifelse(allcat == "immediate.noBECCS", "noBECCS", allcat)) %>% 
  mutate(allcat=ifelse(allcat == "immediate.limBio", "limBio", allcat)) %>% 
  mutate(allcat=ifelse(allcat == "immediate.lowEI", "lowEI", allcat)) %>% 
  mutate(allcat=ifelse(grepl("delay2030", allcat), "delay2030", allcat)) %>% 
  mutate(allcat=factor(allcat, 
                       levels=c("optimal", "lowEI", "limBio", "noCCS", "noBECCS", "delay2030"), 
                       labels=c("Default", "Low energy intensity", "Limited bioenergy", "No CCS", "No BECCS", "Delayed action until 2030"), ordered=TRUE)) %>% 
  mutate(value = as.numeric(value))

#-- Split between world and regional data -------------
v_data_timeTechTemp_world <- v_data_timeTechTemp %>% 
  filter(region == "World")
v_data_timeTechTemp_regional <- v_data_timeTechTemp %>% 
  filter(region != "World")

#== OTHER DATASETS ===================================
# RCP data
load(u_path_rcpdata)


# Global Carbon Project (1959-2015)
data_gcp <- process_gcp(u_path_gcpdata)

historicalCO2Budget <- data_gcp  %>% 
  mutate(value = value/1000) %>% 
  group_by(model,scenario,region,variable,unit) %>% 
  arrange(period) %>% 
  mutate(value = cumsum(value)) %>% 
  ungroup() %>% 
  filter(variable == "Emissions|CO2", period %in% c(1990, 1995, 2000, 2005, 2010, 2015))

hco2b_1990 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 1990)]
hco2b_1995 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 1995)]
hco2b_2000 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 2000)]
hco2b_2005 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 2005)]
hco2b_2010 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 2010)]
hco2b_2015 <- historicalCO2Budget$value[which(historicalCO2Budget$period == 2015)]

historicalCO2Budget <- historicalCO2Budget %>% 
  mutate(diff_1990 = value - hco2b_1990) %>% 
  mutate(diff_1995 = value - hco2b_1995) %>% 
  mutate(diff_2000 = value - hco2b_2000) %>% 
  mutate(diff_2005 = value - hco2b_2005) %>% 
  mutate(diff_2010 = value - hco2b_2010) %>% 
  mutate(diff_2015 = value - hco2b_2015)


#== SAVE DATA ================================
save(v_dataAR5, v_dataALR, v_dataL13, v_dataSR15, 
     v_data_tempTargets, v_data_tempTargets_world, v_data_tempTargets_regional, 
     v_data_timeTechTemp, v_data_timeTechTemp_world, v_data_timeTechTemp_regional, data_rcp, data_gcp, 
     file=paste0("data/dataAll_", format(Sys.Date(), format="%Y%m%d"),".RData"))