#' Tidy AR5 data
#' 
#' Remove unnecessary time steps, levels ...
#'     
#' @param i_data the ar5data or a sub-selection of ar5data.
#' 
#' @return A quitte object containing the baseline scenarios. 
#' 
#' @examples
#' myData <- ar5clean()
#' 
#' @author Jerome Hilaire \email{hilaire@@pik-potsdam.de}
#' 
#' @keywords clean
#' 
#' @import dplyr tidyr magclass
#' @export


ar5clean <- function(i_data=NULL) {
  
  
  
  if (is.null(i_data)) i_data <- ar5data    
  
  # Clean data
  i_data$model    <- factor(i_data$model,    levels=unique(i_data$model))
  i_data$scenario <- factor(i_data$scenario, levels=unique(i_data$scenario))
  i_data$region   <- factor(i_data$region,   levels=unique(i_data$region))
  i_data$variable <- factor(i_data$variable, levels=unique(i_data$variable))
  
  o_sel <- i_data %>%
      filter(period %in% c(2005,seq(2010,2100,10)))
  return(o_sel)
}