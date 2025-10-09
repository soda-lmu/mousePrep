#' Remove cases from dataset 
#'
#' This function takes the data and the specified participant ID, session ID, the selected column,
#' which entails the filter variable, the filter variable, the filter criteria and returns the filtered 
#' data frame with the removed cases. 
#' 
#' Can be used to remove cases where e.g.:
#'  - the window was resized
#'  - touch devices were used
#'  - the answer was not recorded in English
#'  - ...
#'
#' @param data A data frame with the mouse movement data. 
#' @param part_id The participant ID.
#' @param sess_id The session or worker ID, identifies the separated session, one participant has multiple IDs. 
#' @param column_rm The column, which is used for the filter selection.
#' @param factor_rm The factor / category which should be removed. 
#' @param criteria For 1: Remove only the session / worker Id, where the criteria are met.
#'                 For 2: Remove all data from the participant, where the criteria are met. 
#' @return A filtered data frame with removed cases. 
#' @examples
#' rm_cases(data, factor_rm = "resize", criteria = 1)
#' rm_cases(data, column_rm = "userAgent_is_touch_capable", factor_rm = TRUE, criteria = 1)
#' @export

rm_cases <- function(data, part_id = "mouseid", sess_id = "workerId", column_rm, factor_rm, criteria) {
  
  if (criteria == "1"){
    # select the ids with where the condition applies 
    resize_mt_ids <- unique(data[[sess_id]][data[[column_rm]] == factor_rm])  
    # remove the selected ids from the dataset 
    new_data <- data[which(!(data[[sess_id]] %in% resize_mt_ids)),]
    
    return(new_data)
    
  } else if (criteria == "2"){
    # select the participant ids with where the condition applies 
    resize_mt_ids <- unique(data[[part_id]][data[[column_rm]] == factor_rm])  
    # remove the selected ids from the dataset 
    new_data <- data[which(!(data[[part_id]] %in% resize_mt_ids)),]
    
    return(new_data)  
    
  } 
}