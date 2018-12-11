#== USER SECTION =========================
source("scripts/user_section.R")


#== INITIALIZE ===========================
source("scripts/init.R")


#== PROCESS DATA =========================
if(u_processDB) {
  rst <- winDialog(type = c("okcancel"), "Warning! You are about to regenerate scenario datasets. This operation could delete old ones. Do you want to continue?")
  if (rst == "OK") {
    source("scripts/process_databases.R")
  } else {
    stop()
  }
} else {
  load(u_path_processedData)
}


#== PLOT DATA ============================
#-- Example #1
v_dataplot <- compute_stats_tempcat(v_data_tempTargets_world, "Emissions|CO2")

plot_ribbons_tempcat(v_dataplot,lower = "q15", upper = "q85", ylab = "CO2 emissions")

#-- Example #2
v_dataplot <- compute_stats_allcat(v_data_timeTechTemp_world %>% 
                                     mutate(tempcat = paste(tempcat)) %>% 
                                     filter(tempcat %in% c("L2C", "M2C")) %>% 
                                     mutate(tempcat = "2C"), "Emissions|CO2")

plot_ribbons_allcat_2C(v_dataplot,lower = "q15", upper = "q85", ylab = "CO2 emissions")
