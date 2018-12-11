#
get_models <- function(i_data) {
  o_models <- sort(unique(i_data$model))
  return(o_models)
}
get_scenarios <- function(i_data) {
  o_scenarios <- sort(unique(i_data$scenario))
  return(o_scenarios)
}
get_variables <- function(i_data) {
  o_variables <- sort(unique(i_data$variable))
  return(o_variables)
}

# Creates a new variable with the temperature goal categories: 1p5C, L2C, M2C, L3C, Other
categorise_tempGoals <- function(i_df, dataset="AR5") {
  
  out <- data.frame()
  
  if (dataset == "AR5") {
    out <- i_df %>% 
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

# IAM scenario functions
append_carbonprice <- function(i_df, i_df_cp) {
  res <- i_df %>% 
    inner_join(i_df_cp %>% filter(variable == "Price|Carbon", period == 2050) %>% select(model,scenario, tempcat, value) %>% 
                 rename(co2price=value),
               by=c("model","scenario","tempcat"))
  
  return(res)
}


library(ggplot2)  ## devtools::install_github("hadley/ggplot2)
library(grid)     ## rasterGrob
library(EBImage)  ## readImage
library(ggthemes) ## theme_minimal

## ##########
## INDEPENDENT CODE TO BE SOURCED:
## ##########
# user-level interface to the element grob
my_axis = function(img) {
  structure(
    list(img=img),
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )
}
# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_custom <- function(element, x,...)  {
  stopifnot(length(x) == length(element$img))
  tag <- names(element$img)
  # add vertical padding to leave space
  g1 <- textGrob(paste0(tag, "\n\n\n\n\n"), x=x, vjust=1.2)
  g2 <- mapply(rasterGrob, x=x, image=element$img[tag], 
               MoreArgs=list(vjust=0.1, interpolate=FALSE,
                             height=unit(3,"lines")),
               SIMPLIFY=FALSE)
  
  gTree(children=do.call(gList, c(list(g1), g2)), cl="custom_axis")
}
# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_axis = heightDetails.custom_axis = function(x, ...)
  unit(6, "lines")
## ##########
## END
## ##########
