get_coal_variables <- function(i_data) {
  
  tmp_coal_variables <- c(
    'Capacity|Electricity|Coal', 'Capacity|Electricity|Coal|w/ CCS', 'Capacity|Electricity|Coal|w/o CCS', 
    'Cumulative Capacity|Electricity|Coal', 'Cumulative Capacity|Electricity|Coal|w/ CCS', 'Cumulative Capacity|Electricity|Coal|w/o CCS',
    'Capital Cost|Electricity|Coal|IGCC|w/o CCS', 'Capital Cost|Electricity|Coal|IGCC|w/ CCS', 
    'Capital Cost|Electricity|Coal|w/o CCS', 'Capital Cost|Gases|Coal|w/o CCS', 'Capital Cost|Hydrogen|Coal|w/ CCS', 'Capital Cost|Hydrogen|Coal|w/o CCS', 'Capital Cost|Liquids|Coal|w/ CCS', 'Capital Cost|Liquids|Coal|w/o CCS',
    'Final Energy|Residential and Commercial|Solids|Coal', 'Final Energy|Solids|Coal', 'Final Energy|Transportation|Liquids|Coal', 
    'Final Energy|Industry|Solids|Coal', 'Final Energy|Non-Energy Use|Coal', 'Final Energy|Solids|Coal', 'Final Energy|Transportation|Solids|Coal', 
    'Investment|Energy Supply|Electricity|Coal', 'Investment|Energy Supply|Electricity|Coal|w/ CCS', 'Investment|Energy Supply|Electricity|Coal|w/o CCS', 'Investment|Energy Supply|Extraction|Coal', 'Investment|Energy Supply|Liquids|Coal and Gas',
    'Price|Primary Energy|Coal', 'Price|Final Energy|Residential|Solids|Coal', 'Price|Secondary Energy|Solids|Coal',
    'Primary Energy|Coal', 'Primary Energy|Coal|Electricity', 'Primary Energy|Coal|w/ CCS', 'Primary Energy|Coal|w/o CCS', 'Primary Energy|Coal|Electricity', 'Primary Energy|Coal|Electricity|w/ CCS', 'Primary Energy|Coal|Electricity|w/o CCS',
    'Resource|Cumulative Extraction|Coal', 
    'Secondary Energy|Electricity|Coal', 'Secondary Energy|Electricity|Coal|w/ CCS', 'Secondary Energy|Electricity|Coal|w/o CCS', 'Secondary Energy|Gases|Coal', 'Secondary Energy|Liquids|Coal', 'Secondary Energy|Solids|Coal', 
    'Trade|Primary Energy|Coal|Volume')
  
  out <- i_data %>% 
    dplyr::filter(variable %in% 
             c("GDP|MER",
               "GDP|PPP",
               "Consumption",
               "Emissions|CH4",
               "Emissions|N2O",
               "Emissions|F-Gases",
               "Emissions|CO2",
               "Emissions|CO2|Fossil Fuels and Industry",
               "Emissions|CO2|Fossil Fuels and Industry|Energy Demand",
               "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Industry",
               "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Residential and Commercial",
               "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Transportation",
               "Emissions|CO2|Fossil Fuels and Industry|Energy Supply",
               "Emissions|CO2|Land Use",
               "Price|Carbon",
               tmp_coal_variables) | 
             grepl("Temperature.Exceedance Probability", variable, variable) | 
             grepl("Temperature.Global Mean", variable, variable) |
             grepl("Primary Energy", variable) | 
             grepl("Secondary Energy", variable) |
             grepl("Investment", variable) | 
             grepl("Total cost", variable) |
             grepl("Policy Cost*", variable)) 
  
  return(out)
}