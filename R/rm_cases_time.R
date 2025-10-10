#' Remove participants or cases from data set based on maximum time spent on a question
#'
#' This function returns a filtered dataset based on the maximum time spend on a question on 
#' either participant or screen level. If the function should only consider the cases, leave 
#' the participant ID empty and only fill screen ID. If the function should consider the time on 
#' participant level, fill in both participant and screen ID.
#' 
#' Can be used to remove cases where time exceeded a certain threshold per question, e.g. 7 minutes. 
#'
#' @param data A data frame with the mouse movement data. 
#' @param part_id The participant ID.
#' @param screen_id The screen ID. 
#' @param time_col The column name of the timestamp.
#' @param max_time The time cutoff specified in minutes. 
#' 
#' @return A filtered data frame with removed cases. 
#' @examples
#' rm_cases_time(df, part_id = "mouseid", screen_id = "workerId", time_col = "timeStamp", max_time = 7)
#' @export

rm_cases_time <- function(data, part_id = FALSE, screen_id = "workerId", time_col, max_time) {
  
  if (is.character(part_id)){
    
    #calculate the time needed for one question
    resp_time <- by(as.numeric(data[[time_col]]), data[[screen_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(workerID = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time/1000 #from milliseconds to seconds
    
    rm_part <- resp_time$workerID[which(resp_time$resp_time<=0|resp_time$resp_time> max_time*60)]
    
    #select the cases needed
    rm_part_uasid <- data[which(data[[screen_id]] %in% rm_part),]
    rm_part_uasid <- unique(rm_part_uasid[[part_id]])
    traces_uasids <- data[which(!data[[part_id]]%in%rm_part_uasid),]
    
    return(traces_uasids) 
    
  } else if (!part_id) {
    
    #calculate the time needed for one question
    resp_time <- by(as.numeric(data[[time_col]]), data[[screen_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(workerID = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time/1000 #from milliseconds to seconds
    
    rm_part <- resp_time$workerID[which(resp_time$resp_time<=0|resp_time$resp_time> max_time*60)]
    
    #select the cases needed
    traces_uasids <- data[which(!data[[screen_id]] %in% rm_part),]
    
    return(traces_uasids)
    
  }

}



