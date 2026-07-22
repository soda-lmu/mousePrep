#' Detect screens with multiple trajectories
#'
#' Detects screens containing more than one trajectory and identifies which
#' trajectory or trajectories should be selected according to the specified
#' criterion.
#'
#' @param data A data frame containing mouse-movement data.
#' @param screen_id Character string giving the name of the unique screen
#'   identifier column. A trajectory should belong to only one screen. If each
#'   user has only one screen, a user ID column may also be used.
#' @param traj_id Character string giving the name of the trajectory identifier
#'   column.
#' @param criteria Character string specifying which trajectories should be
#'   selected when a screen contains multiple trajectories:
#'   \itemize{
#'     \item \code{"all"} selects all trajectories belonging to screens with
#'       multiple trajectories.
#'     \item \code{"first"} selects the first recorded trajectory for each
#'       screen with multiple trajectories.
#'     \item \code{"last"} selects the last recorded trajectory for each
#'       screen with multiple trajectories.
#'     \item \code{"long"} selects the trajectory with the longest duration for
#'       each screen with multiple trajectories, based on the range of values
#'       in \code{time_col}.
#'   }
#' @param time_col Optional character string giving the name of the timestamp
#'   column. Required when \code{criteria = "long"}.
#'
#' @return The input data frame with two additional logical columns:
#'   \itemize{
#'     \item \code{multiple_workers}: \code{TRUE} when a screen contains more
#'       than one trajectory and \code{FALSE} otherwise.
#'     \item \code{selected_trajectory}: \code{TRUE} for selected trajectories,
#'       \code{FALSE} for non-selected trajectories on screens with multiple
#'       trajectories, and \code{NA} for screens with only one trajectory.
#'   }
#'
#'   Rows with missing screen or trajectory identifiers are removed. When
#'   \code{criteria = "long"}, trajectories for which all timestamps are
#'   missing are also removed.
#'
#' @examples
#'
#' new_df <- multiple_traj(data = df,screen_id = "screen",traj_id = "workerId",criteria = "long",time_col = "timestamps")
#'
#' @export

multiple_traj <- function(data, screen_id, traj_id, criteria,
                          time_col = NULL) {
  
  if (!traj_id %in% names(data)) {
    stop("The trajectory ID column is missing: ", traj_id)
  }
  
  if (!screen_id %in% names(data)) {
    stop("Unique screen ID is missing: ", screen_id)
  }
  
  # Remove rows with missing screen or trajectory IDs
  missing_id <- is.na(data[[screen_id]]) |
    trimws(as.character(data[[screen_id]])) == "" |
    is.na(data[[traj_id]]) |
    trimws(as.character(data[[traj_id]])) == ""
  
  if (any(missing_id)) {
    warning(
      "Removing ", sum(missing_id),
      " rows with a missing screen ID or trajectory ID."
    )
    
    data <- data[!missing_id, , drop = FALSE]
  }
  
  
  # Check whether a trajectory belongs to multiple screens
  screens_per_traj <- tapply(
    data[[screen_id]],
    data[[traj_id]],
    function(x) length(unique(x))
  )
  
  invalid_trajs <- names(screens_per_traj)[screens_per_traj > 1]
  
  if (length(invalid_trajs) > 0) {
    warning(
      "The following trajectories belong to more than one screen "
    )
  }
  
  
  # Mark screens that contain more than one trajectory
  data$multiple_workers <- ave(
    data[[traj_id]],
    data[[screen_id]],
    FUN = function(x) length(unique(x))
  ) > 1
  
  
  if (criteria == "all") {
    
    data$selected_trajectory <- NA
    data$selected_trajectory[data$multiple_workers] <- TRUE
    
    return(data)
    
  } else if (criteria == "first") {
    
    selected_trajs <- ave(
      as.character(data[[traj_id]]),
      data[[screen_id]],
      FUN = function(x) x[1]
    )
    
    data$selected_trajectory <- NA
    data$selected_trajectory[data$multiple_workers] <- FALSE
    
    data$selected_trajectory[
      data$multiple_workers &
        as.character(data[[traj_id]]) == selected_trajs
    ] <- TRUE
    
    return(data)
    
  } else if (criteria == "last") {
    
    selected_trajs <- ave(
      as.character(data[[traj_id]]),
      data[[screen_id]],
      FUN = function(x) x[length(x)]
    )
    
    data$selected_trajectory <- NA
    data$selected_trajectory[data$multiple_workers] <- FALSE
    
    data$selected_trajectory[
      data$multiple_workers &
        as.character(data[[traj_id]]) == selected_trajs
    ] <- TRUE
    
    return(data)
    
  } else if (criteria == "long") {
    
    if (is.null(time_col)) {
      stop("time_col must be provided when criteria = 'long'.")
    }
    
    if (!time_col %in% names(data)) {
      stop("The time column is missing: ", time_col)
    }
    
    
    timestamps <- suppressWarnings(
      as.numeric(as.character(data[[time_col]]))
    )
    
    # Identify trajectories with no valid timestamps
    all_time_missing <- ave(
      is.na(timestamps),
      data[[traj_id]],
      FUN = all
    )
    
    if (any(all_time_missing)) {
      removed_trajs <- unique(data[[traj_id]][all_time_missing])
      
      warning(
        "Removing ", length(removed_trajs),
        " trajectories because all timestamps are missing "
      )
      
      data <- data[!all_time_missing, , drop = FALSE]
      timestamps <- timestamps[!all_time_missing]
    }
    
    
    resp_time <- tapply(
      timestamps,
      data[[traj_id]],
      function(x) diff(range(x, na.rm = TRUE))
    )
    
    data$resp_time <- as.numeric(
      resp_time[as.character(data[[traj_id]])]
    )
    
    
    # Recalculate because trajectories with missing timestamps may be removed
    data$multiple_workers <- ave(
      data[[traj_id]],
      data[[screen_id]],
      FUN = function(x) length(unique(x))
    ) > 1
    
    max_time <- ave(
      data$resp_time,
      data[[screen_id]],
      FUN = max
    )
    
    data$selected_trajectory <- NA
    data$selected_trajectory[data$multiple_workers] <- FALSE
    
    data$selected_trajectory[
      data$multiple_workers &
        data$resp_time == max_time
    ] <- TRUE
    
    return(data)
    
  } else {
    
    stop("criteria must be one of: all, first, last, or long.")
  }
}
