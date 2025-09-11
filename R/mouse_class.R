#' Classification of touch/mouse users.
#'
#' This function selects cases based on the amount of recorded data points per session 
#' per participant. Default value is >50 for mouse users and <10 for touch devices. Cases between 
#' 10 and 50 data points can be manually inspected, e.g. by checking the trajectory plots.  
#' 
#' Define only max_cutoff, when the questionable devices need to be removed, returns filtered data set. 
#' When min_cutoff is also specified, the cases in the specified range will be returned for futher inspection. 
#'
#' @param data A (sl_cases()) data frame with only mouse movement data. 
#' @param sess_id The grouping variable, like a session or worker ID.
#' @param count_var A column in the data set used to count the cases per sess_id.
#' @param max_cutoff The cutoff value for mouse users. 
#' @param min_cutoff the cutoff value for touch users, should not be the same as max_cutoff. 
#' @return  
#' @examples
#' df_mv <- sl_cases(df, column_sl = "type", factor_sl = "mousemove")
#' df_mv <- mouse_class(df_mv, min_cutoff = 10)
#' @export

mouse_class <- function(data, sess_id = "workerId", count_var = "timeStamp", max_cutoff = 50, min_cutoff = FALSE) {
  
  # return a data set with more than the number of specified cases in max_cutoff
  if (min_cutoff == FALSE) {
    # get the number of data point per session
    event_counts <- by(data[[count_var]], data[[sess_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are larger than max_cutoff
    questionable_ids <- event_counts[which( event_counts$event_counts >= max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[sess_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
  # returns a data set with questionable devices in the specified cutoff range for further testing 
  } else if (is.numeric(min_cutoff)){
    # get the number of data point per session
    event_counts <- by(data[[count_var]], data[[sess_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are in the specified range 
    questionable_ids <- event_counts[which(event_counts$event_counts> min_cutoff &
                                             event_counts$event_counts< max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[sess_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
  }
}
