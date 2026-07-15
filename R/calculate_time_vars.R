#' Calculate movement time variables
#'
#' Computes initiation time, response time, and move time for each participant
#' or trial ID. Initiation time is the earliest timestamp within each group,
#' response time is the latest timestamp, and move time is the difference
#' between response time and initiation time.
#'
#' @param data Any mouse trajectory data frame or tibble.
#' @param id A character string naming the participant or trial ID column.
#'   Defaults to `"mt_id"`.
#' @param timestamps A character string naming the timestamp column, measured
#'   in milliseconds. Defaults to `"timestamps"`.
#'
#' @return A data frame with one row per `id` and three computed variables:
#'   `RT`, `initiation_time`, and `move_time`.
#'
#' @examples
#' # time_data <- calculate_time_vars(df)
#'
#' @export
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom rlang abort
calculate_time_vars <- function(data, id = "mt_id", timestamps = "timestamps"){
  
  
  required <- c(id, timestamps)
  missing_cols <- setdiff(required, names(data))
  
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    )
  }
  
  if(!is.numeric(data[[timestamps]])){
    rlang::abort("Timestamp must be numeric")
  }
  
  data <- data %>%
    filter(!is.na(.data[[timestamps]])) %>%
    group_by(.data[[id]]) %>%
    summarise(
      RT = max(.data[[timestamps]]),
      initiation_time = min(.data[[timestamps]]),
      .groups = "drop"
    ) %>%
    mutate(move_time = RT - initiation_time)
  
  if(any(data$move_time < 0, na.rm = TRUE)){
    rlang::warn("Some move times are negative. Check timestamp ordering")
  }                        
  
  data
}
