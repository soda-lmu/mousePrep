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
#'     \item \code{"all"} – remove the complete participant if multiple trajectories are present.
#'     \item \code{"first"} – keep only the first recorded trajectory per screen.
#'     \item \code{"last"} – keep only the last recorded trajectory per screen.
#'     \item \code{"long"} – keep only the longest trajectory per screen 
#'     (based on the number of recorded data points).
#'   }
#' 
#' @return Initial dataframe with an indicator column names "one_obs_per_screen"
#' @examples
#' 
#' 
#' @export

multiple_traj <- function(data, part_id,  quest_id, screen_id = "mt_id", criteria){
  
  
  if (criteria == "all") {
    
    
    
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
    
    
  }
  
}

