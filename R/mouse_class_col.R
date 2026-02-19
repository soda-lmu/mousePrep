#' Filter mouse movement cases by number of recorded data points and add additional column
#'
#' This function classifies cases as mouse or touch users based on the number 
#' of recorded movement data points per screen (using the specified timestamp variable).
#' For each screen ID, the number of observations is counted. By default,
#' screens with 50 or more data points are classified as mouse users ("is_mouse"),
#' and those with fewer than 50 data points as touch users ("is_touch").
#' The original dataset is returned with an additional indicator column.
#'
#'
#' @param data A (sl_cases()) data frame with only mouse movement data.
#' @param part_id The ID on participant level.
#' @param screen_id The ID on screen level.
#' @param count_var A column in the dataset used to count the cases per screen_id.
#' @param max_cutoff The cutoff value for mouse users. 
#' @param level Character string, either \code{"screen"} or \code{"participant"}.
#'   If \code{"screen"}, classification is performed at the screen level,
#'   meaning only screens with fewer than \code{max_cutoff} data points are
#'   flagged as \code{"is_touch"}.
#'
#'   If \code{"participant"}, classification is performed at the participant level:
#'   if any screen of a participant has fewer than \code{max_cutoff} data points,
#'   all observations of that participant are flagged as \code{"is_touch"}.
#' 
#' @return An object of class `mouse_class`. 
#' @examples
#' df_mv <- sl_cases(df, column_sl = "type", factor_sl = "mousemove")
#' df_mv <- mouse_class_col(df_mv, max_cutoff = 50)
#' @export

mouse_class_col <- function(data, part_id = NULL, screen_id = "mt_id", count_var = "timestamps", max_cutoff = 50, level = "screen") {
  
  # return complete dataset with an indicator column for touch or mouse devices 
    
    # get the number of data point per screen
    event_counts <- by(data[[count_var]], data[[screen_id]], length)
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are larger than max_cutoff
    questionable_ids <- event_counts[which(event_counts$event_counts <= max_cutoff),]
    
    if(level == "screen") {
    data$mouse_touch <- ifelse(data[[screen_id]] %in% questionable_ids$workerId, "is_touch", "is_mouse")
    
    
    return(data)
    
  } else if (level == "participant"){
  
    flagged_participants <- unique(data[[part_id]][data[[screen_id]] %in% questionable_ids$workerId])
    
    data$mouse_touch <- ifelse(data[[part_id]] %in% flagged_participants,"is_touch", "is_mouse")
    
    return(data)
    
  }
}
