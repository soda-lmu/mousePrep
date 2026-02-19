#' Remove cases from a dataset
#'
#' This function removes cases from a dataset based on a specified filter variable
#' and criterion. Filtering can be applied either at the screen (case) level or at
#' the participant level.
#'
#' The function can be used to exclude cases where, for example:
#' \itemize{
#'   \item the browser window was resized,
#'   \item a touch device was used,
#'   \item the response language was not English,
#'   \item other exclusion criteria apply.
#' }
#'
#' @param data A data frame containing mouse movement data.
#' @param part_id Column name of the participant identifier.
#' @param screen_id Column name of the screen or worker identifier. One participant
#'   can have multiple screen IDs.
#' @param column_rm Column name of the variable used for filtering.
#' @param factor_rm The value or category that should be removed.
#' @param criteria Integer indicating the removal level:
#'   \describe{
#'     \item{1}{Remove only the screen/worker ID where the criterion is met.}
#'     \item{2}{Remove all data from the participant where the criterion is met.}
#'   }
#'
#' @return A filtered data frame with the specified cases removed.
#' @examples
#' rm_cases(data, column_rm = "userAgent_is_touch_capable", factor_rm = TRUE, criteria = 1)
#' 
#' @export


rm_cases <- function(data, part_id = "mouseid", screen_id = "mt_id", column_rm, factor_rm, criteria) {
  
  if (criteria == "1"){
    # select the ids with where the condition applies 
    resize_mt_ids <- unique(data[[screen_id]][data[[column_rm]] == factor_rm])  
    # remove the selected ids from the dataset 
    new_data <- data[which(!(data[[screen_id]] %in% resize_mt_ids)),]
    
    return(new_data)
    
  } else if (criteria == "2"){
    # select the participant ids with where the condition applies 
    resize_mt_ids <- unique(data[[part_id]][data[[column_rm]] == factor_rm])  
    # remove the selected ids from the dataset 
    new_data <- data[which(!(data[[part_id]] %in% resize_mt_ids)),]
    
    return(new_data)  
    
  } 
}