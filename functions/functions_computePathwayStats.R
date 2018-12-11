
compute_stats_tempcat <- function(i_df, i_var) {
  res <- i_df %>% 
    filter(variable == i_var) %>% 
    group_by(tempcat,period) %>% 
    summarise(
      min  = min(value, na.rm=TRUE), 
      q05  = quantile(value, 0.05, na.rm=TRUE), 
      q10  = quantile(value, 0.10, na.rm=TRUE), 
      q15  = quantile(value, 0.15, na.rm=TRUE), 
      q25  = quantile(value, 0.25, na.rm=TRUE), 
      med  = median(value, na.rm=TRUE), 
      mean = mean(value, na.rm=TRUE), 
      q75  = quantile(value, 0.75, na.rm=TRUE), 
      q85  = quantile(value, 0.85, na.rm=TRUE), 
      q90  = quantile(value, 0.90, na.rm=TRUE), 
      q95  = quantile(value, 0.95, na.rm=TRUE), 
      max  = max(value, na.rm=TRUE),
      count=n()) %>% 
    ungroup()
  
  return(res)
}

compute_stats_tempcat_reg <- function(i_df, i_var) {
  res <- i_df %>% 
    filter(variable == i_var) %>% 
    group_by(tempcat,region, period) %>% 
    summarise(
      min  = min(value, na.rm=TRUE), 
      q05  = quantile(value, 0.05, na.rm=TRUE), 
      q10  = quantile(value, 0.10, na.rm=TRUE), 
      q15  = quantile(value, 0.15, na.rm=TRUE), 
      q25  = quantile(value, 0.25, na.rm=TRUE), 
      med  = median(value, na.rm=TRUE), 
      mean = mean(value, na.rm=TRUE), 
      q75  = quantile(value, 0.75, na.rm=TRUE), 
      q85  = quantile(value, 0.85, na.rm=TRUE), 
      q90  = quantile(value, 0.90, na.rm=TRUE), 
      q95  = quantile(value, 0.95, na.rm=TRUE), 
      max  = max(value, na.rm=TRUE),
      count=n()) %>% 
    ungroup()
  
  return(res)
}

compute_stats_allcat <- function(i_df, i_var) {
  res <- i_df %>% 
    filter(variable == i_var) %>% 
    group_by(allcat,tempcat,region,period) %>% 
    summarise(
      min  = min(value, na.rm=TRUE), 
      q05  = quantile(value, 0.05, na.rm=TRUE), 
      q10  = quantile(value, 0.10, na.rm=TRUE), 
      q15  = quantile(value, 0.15, na.rm=TRUE), 
      q25  = quantile(value, 0.25, na.rm=TRUE), 
      med  = median(value, na.rm=TRUE), 
      mean = mean(value, na.rm=TRUE), 
      q75  = quantile(value, 0.75, na.rm=TRUE), 
      q85  = quantile(value, 0.85, na.rm=TRUE), 
      q90  = quantile(value, 0.90, na.rm=TRUE), 
      q95  = quantile(value, 0.95, na.rm=TRUE), 
      max  = max(value, na.rm=TRUE),
      count=n()) %>% 
    ungroup()
  
  return(res)
}

compute_cumulate <- function(i_df, i_var, periods=NULL) {
  res <- i_df %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(variable == i_var) %>% 
    group_by(model, scenario, region, variable, tempcat) %>% 
    arrange(period) %>% 
    mutate(dt = (period - lag(period, default = 2005))) %>% 
    mutate(cum = dt*(value + lag(value, default = 0))/2) %>% 
    mutate(value = cumsum(cum)/1000) %>% 
    dplyr::select(-`dt`, -`cum`) %>% 
    mutate(unit="Gt(CO2)") %>% 
    dplyr::select(model,scenario,region,variable,unit,period,value,tempcat)
  
  if (!is.null(periods)) {
    res <- res %>% 
      filter(period %in% periods)
  }
  
  return(res)
}

compute_cumulate_allcat <- function(i_df, i_var, periods=NULL) {
  res <- i_df %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(variable == i_var) %>% 
    group_by(model, scenario, region, variable, tempcat, allcat) %>% 
    arrange(period) %>% 
    mutate(dt = (period - lag(period, default = 2005))) %>% 
    mutate(cum = dt*(value + lag(value, default = 0))/2) %>% 
    mutate(value = cumsum(cum)/1000) %>% 
    select(-dt, -cum) %>% 
    mutate(unit="Gt(CO2)") %>% 
    select(model,scenario,region,variable,unit,period,value,tempcat,allcat)
  
  if (!is.null(periods)) {
    res <- res %>% 
      filter(period %in% periods)
  }
  
  return(res)
}

compute_maxDecadalRate <- function(i_df, i_var) {
  res <- i_df %>% 
    filter(variable == i_var) %>% 
    group_by(model, scenario, tempcat, variable) %>% 
    arrange(period) %>% 
    mutate(decadal_rate = value - lag(value, default=0)) %>% 
    ungroup() %>% 
    select(-value)
  
  res <- res %>% 
    inner_join(res %>% 
                 group_by(model, scenario, tempcat, variable) %>% 
                 arrange(period) %>% 
                 summarize(max_decadal_rate=max(decadal_rate)) %>% 
                 ungroup(),
               by=c("model","scenario","tempcat","variable")) %>% 
    filter(decadal_rate == max_decadal_rate, period != 2005) %>% 
    filter(max_decadal_rate != 0) %>% 
    select(-decadal_rate)
  
  return(res)
}

compute_maxDeployRate <- function(i_df, i_var, i_start=2020, i_end=2050) {
  res <- i_df %>% 
    filter(variable == i_var, period >= i_start & period <= i_end) %>% 
    group_by(model, scenario, tempcat, variable) %>% 
    arrange(period) %>% 
    mutate(avgDeployRate_annual  = (value - lag(value, default=0))/(i_end-i_start)) %>%
    mutate(avgDeployRate_decadal = (value - lag(value, default=0))/(i_end-i_start)/10) %>% 
    ungroup() %>% 
    filter(period != i_start) %>% 
    select(-value)
  
  return(res)
}

compute_avgDeployRate <- function(i_df, i_var, i_start=2020, i_end=2050) {
  res <- i_df %>% 
    filter(variable == i_var, period %in% c(i_start, i_end)) %>% 
    group_by(model, scenario, tempcat, variable) %>% 
    arrange(period) %>% 
    mutate(avgDeployRate_annual  = (value - lag(value, default=0))/(i_end-i_start)) %>%
    mutate(avgDeployRate_decadal = (value - lag(value, default=0))/(i_end-i_start)/10) %>% 
    ungroup() %>% 
    filter(period != i_start) %>% 
    select(-value)
  
  return(res)
}

compute_avgDeployRate2030250_relative <- function(i_df, i_var) {
  res <- i_df %>% 
    filter(variable == i_var, period %in% c(2030, 2050)) %>% 
    group_by(model, scenario, tempcat, variable) %>% 
    arrange(period) %>% 
    mutate(avgDeployRate_annual  = ((value - lag(value, default=0))/lag(value, default=0)/20)*100) %>%
    mutate(avgDeployRate_decadal = ((value - lag(value, default=0))/lag(value, default=0)/2)*100) %>% 
    ungroup() %>% 
    filter(period != 2030) %>% 
    select(-value)
  
  return(res)
}