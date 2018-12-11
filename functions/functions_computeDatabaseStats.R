# Scenario statistics functions
compute_nbscen           <- function(i_df) {
  res <- length(unique(i_df$scenario))
  return(res)
}
get_scen                 <- function(i_df) {
  res <- paste(unique(i_df$scenario), collapse=", ")
  return(res)
}
compute_nbmod            <- function(i_df) {
  res <- length(unique(i_df$model))
  return(res)
}
get_mod                  <- function(i_df) {
  res <- paste(unique(i_df$model), collapse=", ")
  return(res)
}
compute_nbscenmod        <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario)
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbmip            <- function(i_df) {
  res <- length(unique(i_df$source))
  return(res)
}
get_mip                  <- function(i_df) {
  res <- paste(unique(i_df$source), collapse=", ")
  return(res)
}
compute_nbbeccs          <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Primary Energy|Biomass|w/ CCS", period == 2100) %>% 
    mutate(beccs = ifelse(value > 1.0, TRUE, FALSE))
  res <- length(which(tmp$beccs == TRUE))
  return(res)
}
compute_nb15             <- function(i_df) {
  res <- NA
  if ("tempcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(tempcat == "1.5°C scenario")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbL20            <- function(i_df) {
  res <- NA
  if ("tempcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(tempcat == "Likely 2.0°C scenario")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbM20            <- function(i_df) {
  res <- NA
  if ("tempcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(tempcat == "Medium 2.0°C scenario")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbL30            <- function(i_df) {
  res <- NA
  if ("tempcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(tempcat == "Likely 3.0°C scenario")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nboT             <- function(i_df) {
  res <- NA
  if ("tempcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(tempcat == "Other scenario")
    #filter(!tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario", "Likely 3.0°C scenario"))
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nboptimal        <- function(i_df) {
  res <- NA
  if ("techcat" %in% names(i_df) && "timingcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(techcat == "default", timingcat == "immediate")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbdelay          <- function(i_df) {
  res <- NA
  if ("timingcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(timingcat == "delay2030")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nblimbio         <- function(i_df) {
  res <- NA
  if ("techcat" %in% names(i_df)) {
    
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(techcat == "limBio")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nblowei          <- function(i_df) {
  res <- NA
  if ("techcat" %in% names(i_df)) {
    
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(techcat == "lowEI")
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbnoccs          <- function(i_df) {
  res <- NA
  if ("techcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(techcat %in% c("noCCS"))
    res <- length(unique(tmp$model.scenario))
  }
  return(res)
}
compute_nbnobeccs        <- function(i_df) {
  res <- NA
  if ("techcat" %in% names(i_df)) {
    tmp <- i_df %>% 
      unite(model.scenario, model, scenario) %>% 
      filter(techcat %in% c("noBECCS"))
    res <- length(unique(tmp$model.scenario))
  }
  
  return(res)
}
compute_nbvarEmi         <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Emissions|CO2", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarEmiLUC      <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Emissions|CO2|Land Use", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarEmiFFI      <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Emissions|CO2|Fossil Fuels and Industry", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarEmiBECCS    <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarCP          <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Price|Carbon", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarPEBio       <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Primary Energy|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarPEBioCCS    <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Primary Energy|Biomass|w/ CCS", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioPow    <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Electricity|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioPowCCS <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Electricity|Biomass|w/ CCS", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioLiq    <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Liquids|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioLiqCCS <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Liquids|Biomass|w/ CCS", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioH2     <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Hydrogen|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioH2CCS  <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Hydrogen|Biomass|w/ CCS", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioGas    <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Gases|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}
compute_nbvarSEBioHeat   <- function(i_df) {
  tmp <- i_df %>% 
    unite(model.scenario, model, scenario) %>% 
    filter(variable == "Secondary Energy|Heat|Biomass", period == 2050, !is.na(value))
  res <- length(unique(tmp$model.scenario))
  return(res)
}

save_stats <- function(i_data, i_name, i_source="", i_url="", i_mip="") {
  
  out <- data.frame(
    Name    = i_name,
    Source  = i_source,
    URL     = i_url,
    NbMod      = compute_nbmod(i_data),
    ModNames   = get_mod(i_data),
    NbScen     = compute_nbscen(i_data),
    ScenNames  = get_scen(i_data),
    NbModScen  = compute_nbscenmod(i_data),
    Nb15       = compute_nb15(i_data),
    NbLikely20 = compute_nbL20(i_data),
    NbMedium20 = compute_nbM20(i_data),
    NbLikely30 = compute_nbL30(i_data),
    NbOtherTem = compute_nboT(i_data),
    NbBECCS    = compute_nbbeccs(i_data),
    NbOptim    = compute_nboptimal(i_data),
    NbNoCCS    = compute_nbnoccs(i_data),
    NbNoBECCS  = compute_nbnobeccs(i_data),
    NbDelay    = compute_nbdelay(i_data),
    NbLimBio   = compute_nblimbio(i_data),
    NbMip      = compute_nbmip(i_data),
    NbVarEmi         = compute_nbvarEmi(i_data),
    NbVarEmiBECCS    = compute_nbvarEmiBECCS(i_data),
    NbVarCPrice      = compute_nbvarCP(i_data),
    # NbVarPEBio       = compute_nbvarPEBio(i_data),
    # NbVarPEBioCCS    = compute_nbvarPEBioCCS(i_data),
    # NbVarSEBioPow    = compute_nbvarSEBioPow(i_data),
    # NbVarSEBioPowCCS = compute_nbvarSEBioPowCCS(i_data),
    # NbVarSEBioLiq    = compute_nbvarSEBioLiq(i_data),
    # NbVarSEBioLiqCCS = compute_nbvarSEBioLiqCCS(i_data),
    # NbVarSEBioH2     = compute_nbvarSEBioH2(i_data),
    # NbVarSEBioH2CCS  = compute_nbvarSEBioH2CCS(i_data),
    # NbVarSEBioGas    = compute_nbvarSEBioGas(i_data),
    # NbVarSEBioHeat   = compute_nbvarSEBioHeat(i_data),
    MipNames   = ifelse(i_name == "AR5", get_mip(i_data), i_mip)
  )
  
}