#' Select cases from a dataset
#'
#' This function selects cases from a dataset based on a specified variable and
#' value and returns a filtered data frame.
#'
#' The function can be used to select cases such as:
#' \itemize{
#'   \item non-touch device users,
#'   \item observations containing mouse movement data,
#'   \item other user-defined selection criteria.
#' }
#'
#' @param data A data frame containing mouse movement data.
#' @param column_sl Column name of the variable used for selection.
#' @param factor_sl The value or category to be selected.
#'
#' @return A filtered data frame containing only the selected cases.
#' @examples
#' sl_cases(data, column_sl = "type", factor_sl = "mousemove")
#' @export


sl_cases <- function(data, column_sl, factor_sl) {
  
    # keep the selected ids from the dataset 
    new_data <- data[data[[column_sl]] == factor_sl,]
    
    return(new_data)
    
}
