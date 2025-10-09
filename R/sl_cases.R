#' Selects cases from dataset 
#'
#' This function selects the specified cases based on the column and the level, return the
#' new data frame. 
#' 
#' Can be used to select cases with e.g.:
#'  - no touch device
#'  - mouse movement data
#'  - ...
#'
#' @param data A data frame with the mouse movement data. 
#' @param column_sl The column, which is used for the filter selection.
#' @param factor_sl The factor / category which is selected. 
#' @return A filtered data frame with removed cases. 
#' @examples
#' sl_cases(data, column_sl = "type", factor_sl = "mousemove")
#' @export

sl_cases <- function(data, column_sl, factor_sl) {
  
    # keep the selected ids from the dataset 
    new_data <- data[data[[column_sl]] == factor_sl,]
    
    return(new_data)
    
}