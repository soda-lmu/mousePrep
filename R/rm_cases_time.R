#' Remove participants or cases based on maximum time spent on a question
#'
#' This function filters a dataset based on the maximum time spent on a question,
#' either at the participant level or at the screen (case) level.  
#' If only a screen ID is provided, filtering is applied at the case level.  
#' If both participant ID and screen ID are provided, 
#' the whole participant is removed if at least one screen exceeds the threshold.
#' If all the timestamps of a screen ID is missing, it is removed.  
#'
#'
#' @details
#' This function can be used to remove cases where the time spent on a question
#' exceeds a predefined threshold. It requires that a screen id doesn't occur for multiple participants.
#' 
#' @param data A data frame with the mouse movement data with screen id unique for participant. 
#' @param time_agg A boolean string specifying if the time has to aggregated at the participant level or not. 
#' If it is marked TRUE, the participant column to aggregate timestamps must be provided. 
#' @param part_id The participant ID.
#' @param screen_id The screen ID. 
#' @param time_col The column name of the timestamp.
#' @param max_time The time cutoff. 
#' 
#' @return A filtered data frame with removed cases. 
#' @examples
#' rm_cases_time(df, time_agg = FALSE, part_id = "mouseid", screen_id = "mt_id", time_col = "timestamps", max_time = 420000)
#' @export
#' @importFrom rlang abort

rm_cases_time <- function(data, time_agg = FALSE, part_id = "none", screen_id = "mt_id", time_col = "timestamps", max_time) {
  
  # Remove screen IDs where all timestamps are missing
  data <- data[!ave(is.na(data[[time_col]]), data[[screen_id]], FUN = all), ]
  
  if (isTRUE(time_agg) && part_id == "none") {
    rlang::abort("Must provide part_id if time_agg is TRUE")
    
  } else if (isTRUE(time_agg) && part_id != "none") {
    
    #calculate the time needed for one question
    resp_time <- by(as.numeric(data[[time_col]]), data[[part_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(part_id = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time 
    
    rm_part <- resp_time$part_id[which(resp_time$resp_time<=0|resp_time$resp_time> max_time)] 
    
    #select the cases needed
    rm_part_uasid <- data[which(data[[part_id]] %in% rm_part),]  
    rm_part_uasid <- unique(rm_part_uasid[[part_id]])
    traces_uasids <- data[which(!data[[part_id]]%in%rm_part_uasid),]
    
    return(traces_uasids)
    
  } else if (time_agg == FALSE && part_id != "none"){
    #calculate the time needed for one question
    resp_time <- by(as.numeric(data[[time_col]]), data[[screen_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(screen_id = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time 
    
    rm_part <- resp_time$screen_id[which(resp_time$resp_time<=0|resp_time$resp_time> max_time)] 
    
    #select the cases needed
    rm_part_uasid <- data[which(data[[screen_id]] %in% rm_part),]  
    rm_part_uasid <- unique(rm_part_uasid[[part_id]])
    traces_uasids <- data[which(!data[[part_id]]%in%rm_part_uasid),]
    
    return(traces_uasids)
    
  }else if (part_id == "none") {
    
    #calculate the time needed for one question
    resp_time <- by(as.numeric(data[[time_col]]), data[[screen_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(screen_id = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time
    
    rm_part <- resp_time$screen_id[which(resp_time$resp_time<=0|resp_time$resp_time> max_time)]
    
    #select the cases needed
    traces_uasids <- data[which(!data[[screen_id]] %in% rm_part),]
    
    return(traces_uasids)
    
  }

}
