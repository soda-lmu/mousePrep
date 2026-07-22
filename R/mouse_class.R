#' Filter trajectories/screens by number of recorded movement events
#'
#' Counts the number of recorded movement events for each trajectory and returns
#' all rows belonging to trajectories whose event counts satisfy the specified
#' cutoff condition.
#'
#' @details
#' The filtering rule depends on which cutoff values are supplied:
#' \itemize{
#' \item When only \code{max_cutoff} is numeric, trajectories with at least
#' \code{max_cutoff} events are returned.
#' \item When both \code{min_cutoff} and \code{max_cutoff} are numeric,
#' trajectories with more than \code{min_cutoff} and fewer than
#' \code{max_cutoff} events are returned.
#' \item When only \code{min_cutoff} is numeric, trajectories with at most
#' \code{min_cutoff} events are returned.
#' }
#'
#' The returned object contains the original rows for the matching trajectories, not
#' only the trajectorie identifiers or event counts.
#'
#' @param data A data frame containing mouse- or touch-movement data.
#' @param traj_id Trajectory identifier column. Defaults to \code{"mt_id"}.
#' @param count_var Character string giving the name of the column whose rows
#' are counted within each trajectory. Defaults to \code{"timestamps"}. Missing values values are dropped.
#' @param max_cutoff Numeric upper cutoff, or \code{FALSE} when no upper cutoff
#' should be used.
#' @param min_cutoff Numeric lower cutoff, or \code{FALSE} when no lower cutoff
#' should be used.
#'
#' @return A filtered data frame containing all observations belonging to
#' trajectories that satisfy the specified event-count condition. If neither
#' cutoff is numeric, the function returns \code{NULL}.
#'
#' @examples
#' df_mv <- sl_cases(df,column_sl = "type",factor_sl = "mousemove" )
#' # Screens with at least 50 events
#' mouse_cases <- mouse_class(df_mv, max_cutoff = 50)
#' # Trajectories with more than 10 and fewer than 50 events
#' intermediate_cases <- mouse_class(df_mv,min_cutoff = 10,max_cutoff = 50)

#'
#' @export

mouse_class <- function(data, traj_id = "mt_id", count_var = "timestamps", max_cutoff = FALSE, min_cutoff = FALSE) {
  
  # return a dataset with more than the number of specified cases in max_cutoff
  if (is.numeric(max_cutoff) && min_cutoff == FALSE) {
    # get the number of data point per trajectory
    event_counts <- by(data[[count_var]], data[[traj_id]], function(x) sum(!is.na(x)))
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are larger than max_cutoff
    questionable_ids <- event_counts[which( event_counts$event_counts >= max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[traj_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
  # returns a dataset with questionable devices in the specified cutoff range for further testing 
  } else if (is.numeric(max_cutoff) && is.numeric(min_cutoff)){
    # get the number of data point per trajectory
    event_counts <- by(data[[count_var]], data[[traj_id]], function(x) sum(!is.na(x)))
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are in the specified range 
    questionable_ids <- event_counts[which(event_counts$event_counts> min_cutoff &
                                             event_counts$event_counts< max_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[traj_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
    # return a dataset with less than the number of specified cases in min_cutoff
  } else if (max_cutoff == FALSE && is.numeric(min_cutoff)){
    
    event_counts <- by(data[[count_var]], data[[traj_id]], function(x) sum(!is.na(x)))
    event_counts <- data.frame(workerId = names(event_counts), event_counts = as.numeric(event_counts))
    
    # check which ids are smaller than min_cutoff
    questionable_ids <- event_counts[which( event_counts$event_counts <= min_cutoff),]
    
    # filter these cases
    traces_uasids <- data[which(data[[traj_id]] %in% questionable_ids$workerId),]
    
    return(traces_uasids)
    
  }
}
