#' Join AR5 datasets
#' 
#' Join the AR5 dataframe (ar5data) with the AR5 scenario description dataframe 
#' (ar5scen).
#'     
#' @param i_data the ar5data or a sub-selection of ar5data.
#' 
#' @return A quitte object containing both AR5 data and AR5 scenario description 
#' 
#' @examples
#' myData <- ar5join()
#' 
#' @author Jerome Hilaire \email{hilaire@@pik-potsdam.de}
#' 
#' @keywords climate, category, selection
#' 
#' @import dplyr tidyr
#' @export


ar5join <- function(i_data=NULL) {
  
  if (is.null(i_data)) i_data <- ar5data    
  
  # Join the two dataframes
  o_sel <- i_data %>% 
    inner_join(ar5scen,
      by=c("model", "scenario")) %>%
    quitte::as.quitte()
  
  return(o_sel)
}