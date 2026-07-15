#' Process mouse-movement data with mousetrap
#'
#' Imports mouse-tracking data in the long format, time-normalizes trajectories, computes
#' derivatives, and calculates summary measures using the `mousetrap` package.
#'
#' @param data A data frame containing raw mouse-tracking data in long format,
#'   structured for use with `mt_import_long()`.
#' @param nsteps An integer giving the number of steps to use for
#'   time-normalization of trajectories.
#' @param hover_threshold A numeric threshold in milliseconds passed to
#'   `mt_measures()` for hover detection. Defaults to `500`.
#'
#' @return A mousetrap object containing: imported raw trajectories,
#' time-normalized trajectories saved as `tn_trajectories_<nsteps>`,
#' derivative measures, summary measures saved as `measures_<nsteps>`.
#' 
#' The time-normalized trajectories are stored under the name
#' `tn_trajectories_<nsteps>`, and the measures are stored under the name
#' `measures_<nsteps>`. The `data` object contains the derivative values.
#'
#' @examples
#' processed_data <- mt_process_data(data = trajectories, nsteps = 101,hover_threshold = 500)
#' 
#'
#' @export
#' @importFrom mousetrap mt_import_long mt_time_normalize mt_derivatives mt_measures
mt_process_data <- function(data, nsteps, hover_threshold = 500, non_na_cols = c("timestamps", "xpos", "ypos"), id = "mt_id", time_col = "timestamps"){
  
  data <- data %>%
    filter(if_all(all_of(non_na_cols), ~ !is.na(.x))) %>%
    group_by(.data[[id]]) %>%
    filter(if_all(all_of(time_col), ~ n_distinct(.x) >= 2)) %>%
    ungroup() %>%
    arrange(.data[[id]], .data[[time_col]]) %>%
    group_by(.data[[id]]) %>%
    mutate(mt_seq = row_number()) %>%
    ungroup()
  
  data <- mt_import_long(data)
  data <- mt_time_normalize(data, 
                            use = 'trajectories', 
                            nsteps = nsteps, 
                            save_as = paste('tn_trajectories', 
                                            nsteps, sep = "_"))
  data <- mt_derivatives(data, 
                         use = paste('tn_trajectories', 
                                     nsteps, sep = "_"))
  data <- mt_measures(data,
                      use = paste('tn_trajectories', nsteps, sep = "_"), 
                      save_as = paste("measures", nsteps, sep = "_"), 
                      hover_threshold = hover_threshold)
  
  
  
  data
}
