#' Filter mouse movement cases by number of recorded data points
#'
#' This function filters participant cases based on the number of recorded 
#' mouse or touch movement data points per screen, using the timestamp variable. 
#' By default, cases with more than 50 data points are classified as mouse users, 
#' and cases with fewer than 10 data points are classified as touch device users. 
#' Cases with 10–50 data points are considered ambiguous and can be manually 
#' inspected (e.g., via trajectory plots).
#'
#' @details
#' If only \code{max_cutoff} is specified, ambiguous devices are removed and the 
#' filtered dataset is returned.  
#' If both \code{min_cutoff} and \code{max_cutoff} are specified, cases within the 
#' specified range are returned for further inspection.  
#' If only \code{min_cutoff} is specified, only ambiguous device cases are returned.

#'
#' @param data A (sl_cases()) data frame with only mouse movement data. 
#' @param screen_id The grouping variable, like a screen or worker ID.
#' @param count_var A column in the dataset used to count the cases per screen_id.
#' @param max_cutoff The cutoff value for mouse users. 
#' @param min_cutoff the cutoff value for touch users, should not be the same as max_cutoff. 
#' @return An object of class `mouse_class`. 
#' @examples
#' df_mv <- sl_cases(df, column_sl = "type", factor_sl = "mousemove")
#' df_mv <- mouse_class(df_mv, max_cutoff = 50, min_cutoff = 10)
#' @export

mouse_class <- function(data, screen_id = "workerId", count_var = "timeStamp", max_cutoff = 50, min_cutoff = 10) {
  
  # return a dataset with more than the number of specified cases in max_cutoff
  if (is.numeric(max_cutoff) && min_cutoff == FALSE) {
    # get the number of data point per screen
    event_counts <- by(data[[count_var]], data[[screen_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are larger than max_cutoff
    questionable_ids <- event_counts[which( event_counts$event_counts >= max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[screen_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
  # returns a dataset with questionable devices in the specified cutoff range for further testing 
  } else if (is.numeric(max_cutoff) && is.numeric(min_cutoff)){
    # get the number of data point per screen
    event_counts <- by(data[[count_var]], data[[screen_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are in the specified range 
    questionable_ids <- event_counts[which(event_counts$event_counts> min_cutoff &
                                             event_counts$event_counts< max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[screen_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
  } else if (max_cutoff == FALSE && is.numeric(min_cutoff)){
    
    event_counts <- by(data[[count_var]], data[[screen_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are smaller than min_cutoff
    questionable_ids <- event_counts[which( event_counts$event_counts <= min_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[screen_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
  }
}
