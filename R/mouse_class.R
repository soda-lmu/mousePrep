#' Classification of touch/mouse users.
#'
#' This function is to filter mouse movement cases based on the amount of recorded data points per screen
#' per participant using the timestamp in the dataset. Default value is >50 for mouse users and <10 for touch devices. Cases between 
#' 10 and 50 data points can be manually inspected, e.g. by checking the trajectory plots. This means we consider mouse users with 
#' less than or equal to 50 data points and greater than or equal to 10 data points for touch device users.  
#' 
#' Define only max_cutoff, when the questionable devices need to be removed, returns filtered dataset. 
#' When min_cutoff is also specified, the cases in the specified range will be returned for further inspection.
#' When only min_cutoff is specified, the function returns the questionable devices only.  
#'
#' @param data A (sl_cases()) data frame with only mouse movement data. 
#' @param screen_id The grouping variable, like a screen or worker ID.
#' @param count_var A column in the dataset used to count the cases per screen_id.
#' @param max_cutoff The cutoff value for mouse users. 
#' @param min_cutoff the cutoff value for touch users, should not be the same as max_cutoff. 
#' @return  
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
