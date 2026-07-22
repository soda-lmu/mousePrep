#' Classify mouse and touch trajectories by event count
#'
#' Counts the number of recorded movement events for each trajectory and classifies
#' trajectories/participants as originating from either a mouse or a touch device.
#'
#' A trajectory with at most \code{max_cutoff} recorded events is classified as
#' \code{"is_touch"}. A screen with more than \code{max_cutoff} events is
#' classified as \code{"is_mouse"}.
#'
#' Classification can be performed either separately for each screen or for the
#' complete participant.
#'
#' @param data A data frame containing mouse-movement data.
#' @param part_id Optional character string giving the name of the participant
#' identifier column. Required when \code{level = "participant"}. 
#' If \code{level = "participant"} is chosen, recommended to remove missing part_id's. 
#' @param traj_id Character string giving the name of the screen or trajectory
#' identifier column. Defaults to \code{"mt_id"}. Recommended to not have missing values. 
#' @param count_var Character string giving the name of the column whose rows
#' are counted within each screen. Defaults to \code{"timestamps"}.
#' @param max_cutoff Numeric cutoff used to distinguish touch from mouse input.
#' Screens with at most this number of events are classified as
#' \code{"is_touch"}. Defaults to \code{50}.
#' @param level Character string specifying the classification level:
#' \itemize{
#' \item \code{"trajectory"} classifies each trajectory independently.
#' \item \code{"participant"} classifies the complete participant as
#' \code{"is_touch"} if at least one of their trajectories has at most
#' \code{max_cutoff} events.
#' }
#'
#' @return The original data frame with an additional character column,
#' \code{mouse_touch}, containing either \code{"is_touch"} or
#' \code{"is_mouse"}.
#'
#' @examples
#' df_mv <- sl_cases(df,column_sl = "type",factor_sl = "mousemove")
#' df_mv <- indicate_mouse_touch(df_mv,traj_id = "mt_id",max_cutoff = 50,level = "screen")
#'
#' @export
indicate_mouse_touch <- function(data, part_id = NULL, traj_id = "mt_id", count_var = "timestamps", max_cutoff = 50, level = "trajectory") {
  
  if (!traj_id %in% names(data)) {
    stop("The trajectory ID column is missing: ", traj_id)
  }
  
  if (!count_var %in% names(data)) {
    stop("The count variable is missing: ", count_var)
  }
  
  if (!level %in% c("trajectory", "participant")) {
    stop("level must be either 'trajectory' or 'participant'.")
  }
  
  if (level == "participant") {
    
    if (is.null(part_id)) {
      stop("part_id must be provided when level = 'participant'.")
    }
    
    if (!part_id %in% names(data)) {
      stop("The participant ID column is missing: ", part_id)
    }
  }
  
  # return complete dataset with an indicator column for touch or mouse devices 
    
    # get the number of data point per screen
    event_counts <- by(data[[count_var]], data[[traj_id]], function(x) sum(!is.na(x)))
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are larger than max_cutoff
    questionable_ids <- event_counts[which(event_counts$event_counts <= max_cutoff),]
    
    if(level == "trajectory") {
    data$mouse_touch <- ifelse(data[[traj_id]] %in% questionable_ids$workerId, "is_touch", "is_mouse")
    
    
    return(data)
    
  } else if (level == "participant"){
  
    flagged_participants <- unique(data[[part_id]][data[[traj_id]] %in% questionable_ids$workerId])
    
    data$mouse_touch <- ifelse(data[[part_id]] %in% flagged_participants,"is_touch", "is_mouse")
    
    return(data)
    
  }
}
