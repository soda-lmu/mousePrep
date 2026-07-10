#' Detect screen with multiple visits
#'
#' Detecting screens with multiple visits and keep a selected screen trajectory
#'
#'
#' @param data A (sl_cases()) data frame with only mouse movement data.
#' @param part_id The ID of the participant.
#' @param quest_id The ID of the question. 
#' @param screen_id The ID on screen level.
#' @param criteria Character string specifying how to handle multiple 
#'   trajectories per screen:
#'   \itemize{
#'     \item \code{"all"} – flag the complete participant if multiple trajectories are present as TRUE.
#'     \item \code{"first"} – flag only the first recorded trajectory per screen.
#'     \item \code{"last"} – flag only the last recorded trajectory per screen.
#'     \item \code{"long"} – flag only the longest trajectory per screen as TRUE
#'     (based on the number of recorded data points).
#'   }
#' 
#' @return Initial dataframe with an indicator column names "one_obs_per_screen"
#' @examples
#' new_df <- multiple_traj(df, "mouseid", "screen", "workerId", "all")
#' 
#' @export

multiple_traj <- function(data, part_id,  quest_id, screen_id = "mt_id", criteria, time_col = NULL){
  
  
  if (criteria == "all") {
    
    data$combi <- paste0(data[[part_id]], '-', data[[quest_id]])
    
    data$multiple_workers <- ave(data[[screen_id]],
                                data$combi,
                                FUN = function(x) length(unique(x))) > 1
    data$combi <- NULL
    return(data)
    
  } else if (criteria == "first"){
    
    unique_trajs_per_screen <- data[[screen_id]][!duplicated(data[, c(part_id, quest_id)])]
    data$one_obs_per_screen <- FALSE
    data$one_obs_per_screen[which(data[[screen_id]] %in% unique_trajs_per_screen)] <- TRUE
    
    return(data)
    
  } else if (criteria == "last"){
    
    unique_trajs_per_screen <- data[[screen_id]][!duplicated(data[, c(part_id, quest_id)], fromLast = TRUE)]
    data$one_obs_per_screen <- FALSE
    data$one_obs_per_screen[which(data[[screen_id]] %in% unique_trajs_per_screen)] <- TRUE
    
    return(data)
    
  } else if (criteria == "long"){
    
    resp_time <- by(as.numeric(data[[time_col]]), data[[screen_id]], function(x){diff(range(x, na.rm = TRUE))})
    resp_time <- data.frame(workerID = names(resp_time), resp_time = as.numeric(resp_time))
    resp_time$resp_time <- resp_time$resp_time/1000 #from milliseconds to seconds
    
    data$combi <- paste0(data[[part_id]], '-', data[[quest_id]])
    data$multiple_workers <- ave(data[[screen_id]],
                                 data$combi,
                                 FUN = function(x) length(unique(x))) > 1
    
    data <- merge(data,
                  resp_time,
                  by.x = screen_id,
                  by.y = "workerID",
                  all.x = TRUE)
    
    max_time <- ave(data$resp_time, data$combi, FUN = max)
    
    data$multiple_workers[data$multiple_workers & data$resp_time == max_time] <- FALSE
    
    data$combi <- NULL
    
    return(data)
  }
  
}


