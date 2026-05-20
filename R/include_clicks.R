#' Include clicks to the preprocessed dataset 
#'
#' This function takes the data, timestamps, initiation time, and click times, and adds a column indicating clicks using one-hot encoding.
#' Clicks are aligned to the closest available timestamp. As a result, the original click data frame and the resulting data frame may differ in row count.
#' 
#'
#' @param dat_complete A data frame with the screen ID, timestamps, and repeated initiation time
#' @param dat_clicks A data frame with the screen ID and timestamps of the clicks 
#' @param screen_id The screen or worker ID, identifies the separated screens, one participant has multiple IDs. The screen ID should be identical for both data frames. 
#' @param time_col The timestamp column.
#' @param type_col The type column for which the click dataframe can be extracted.
#' @param click_label The factor to extract click dataframe.
#' @param binary Optional to keep an indicator if there are 1 or more clicks. 
#' 
#' @return The original data frame with an additional click column. 
#' 
#' 
#' @examples
#' click_data <- include_clicks(df_complete, df_click, click_column = "timestamps")
#' @export

include_clicks <- function(dat_complete,
                                dat_clicks,
                                screen_id = "mt_id",
                                time_col = "timestamps",
                                click_time_col = "timestamps",
                                binary = FALSE) {
  
  clicks <- dat_clicks %>%
    select(
      all_of(screen_id),
      click_time = all_of(click_time_col)
    ) %>%
    distinct() %>%   
    filter(!is.na(click_time)) %>%
    group_by(.data[[screen_id]]) %>%
    mutate(click_id = row_number()) %>%
    ungroup()
  
  traj <- dat_complete %>%
    select(
      all_of(screen_id),
      all_of(time_col)
    ) %>%
    distinct() %>%
    filter(!is.na(.data[[time_col]])) %>%
    group_by(.data[[screen_id]]) %>%
    mutate(
      initiation_time = min(.data[[time_col]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      traj_row = row_number(),
      trajectory_time = .data[[time_col]] + initiation_time
    )
  
  matched_clicks <- clicks %>%
    inner_join(
      traj %>% select(all_of(screen_id), traj_row, trajectory_time),
      by = screen_id
    ) %>%
    mutate(distance = abs(trajectory_time - click_time)) %>%
    group_by(.data[[screen_id]], click_id) %>%
    slice_min(distance, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  click_summary <- matched_clicks %>%
    count(.data[[screen_id]], traj_row, name = "click") %>%
    mutate(click = if (binary) as.integer(click > 0) else click)
  
  result <- traj %>%
    left_join(click_summary, by = c(screen_id, "traj_row")) %>%
    mutate(click = dplyr::coalesce(click, 0L)) %>%
    select(-traj_row, -trajectory_time)
  
  return(result)
}
