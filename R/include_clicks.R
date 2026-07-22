#' Align clicks with trajectory timestamps
#'
#' Matches click events to the nearest available timestamp within the same
#' screen or trajectory and adds a click-count column to the preprocessed
#' trajectory data.
#'
#' For each click, the function finds the trajectory timestamp with the smallest
#' absolute time difference. Multiple clicks may be assigned to the same
#' trajectory row. Because duplicate timestamps are removed before matching,
#' the returned data may contain fewer rows than \code{data_complete}.
#'
#' @param data_complete A data frame containing mouse movement data.
#'   It must include the columns specified by \code{screen_id} and
#'   \code{time_col}.
#' @param data_clicks A data frame containing the click events. It must include
#'   the columns specified by \code{screen_id} and \code{click_time_col}.
#' @param screen_id Character string giving the name of the column that
#'   identifies a screen or trajectory. The same identifier column must be
#'   present in both input data frames. 
#' @param time_col Character string giving the name of the timestamp column in
#'   \code{data_complete}. Defaults to \code{"timestamps"}.
#' @param click_time_col Character string giving the name of the click timestamp
#'   column in \code{data_clicks}. Defaults to \code{"timestamps"}.
#' @param binary Logical. If \code{FALSE}, the \code{click} column contains the
#'   number of clicks assigned to each trajectory row. If \code{TRUE}, it is a
#'   binary indicator equal to \code{1} when at least one click was assigned and
#'   \code{0} otherwise. Defaults to \code{FALSE}.
#'
#' @return A data frame containing one row per distinct combination of
#'   \code{screen_id} and \code{time_col}, together with:
#'   \itemize{
#'     \item \code{initiation_time}: the minimum timestamp within the screen.
#'     \item \code{click}: the number of matched clicks, or a binary click
#'       indicator when \code{binary = TRUE}.
#'   }
#'
#' @examples
#' click_data <- include_clicks(data_complete = df_complete,data_clicks = df_clicks,screen_id = "mt_id",
#' time_col = "timestamps",click_time_col = "timestamps")

#'
#' @export


include_clicks <- function(data_complete,
                           data_clicks,
                           screen_id ,
                           time_col = "timestamps",
                           click_time_col = "timestamps",
                           binary = FALSE) {
  
  clicks <- data_clicks %>%
    select(all_of(screen_id),
      click_time = all_of(click_time_col)) %>%
    distinct() %>%   
    filter(!is.na(click_time)) %>%
    group_by(.data[[screen_id]]) %>%
    mutate(click_id = row_number()) %>%
    ungroup()
  
  traj <- data_complete %>%
    select(all_of(c(screen_id, time_col))) %>%
    distinct() %>%
    filter(!is.na(.data[[time_col]])) %>%
    group_by(.data[[screen_id]]) %>%
    mutate(
      initiation_time = min(.data[[time_col]], na.rm = TRUE)
    ) %>%
    arrange(.data[[screen_id]], .data[[time_col]]) %>%
    mutate(
      traj_row = row_number(),
      trajectory_time = .data[[time_col]] + initiation_time
    ) %>%
    ungroup()
  
  matched_clicks <- clicks %>%
    inner_join(
      traj %>% select(all_of(screen_id), traj_row, trajectory_time),
      by = screen_id) %>%
    mutate(distance = abs(trajectory_time - click_time)) %>%
    group_by(.data[[screen_id]], click_id) %>%
    slice_min(distance, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  click_summary <- matched_clicks %>%
    count(.data[[screen_id]], traj_row, name = "click") %>%
    mutate(click = if (binary) as.integer(click > 0) else click)
  
  result <- traj %>%
    left_join(click_summary, by = c(screen_id, "traj_row")) %>%
    mutate(click = replace_na(click, 0L)) %>%
    select(-traj_row, -trajectory_time)
  
  return(result)
}
