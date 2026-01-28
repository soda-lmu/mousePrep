# R/rm_cases_string.R

#' Remove cases from a mouse-movement dataset
#'
#' Removes rows associated with sessions (worker IDs) or participants when a
#' filter condition is met (e.g., touch device used, window resized, non-English).
#'
#' This function is "tidyverse-friendly":
#' - You can pass column names **unquoted** (e.g., `workerId`) or as **strings**
#'   (e.g., `"workerId"`).
#' - Uses `.data[[...]]` to avoid NSE notes in package checks.
#'
#' @param data A data frame containing mouse movement data.
#' @param column_rm Column used for the exclusion criterion (unquoted or string).
#' @param factor_rm Value(s) in `column_rm` that trigger exclusion (vector allowed).
#' @param criteria One of:
#'   \itemize{
#'     \item `"session"`: remove only the affected sessions (worker IDs)
#'     \item `"participant"`: remove all rows for affected participants
#'   }
#' @param part_id Participant ID column (default `"mouseid"`; unquoted or string).
#' @param sess_id Session/worker ID column (default `"workerId"`; unquoted or string).
#'
#' @return A tibble/data.frame with excluded rows removed.
#'
#' @examples
#' # Remove only sessions where touch-capable devices were used
#' # rm_cases_string(uas_data, column_rm = userAgent_is_touch_capable, factor_rm = TRUE,
#' #          criteria = "session")
#'
#' # Remove entire participants if any of their sessions used a touch device
#' # rm_cases_string(uas_data, column_rm = "userAgent_is_touch_capable", factor_rm = TRUE,
#' #          criteria = "participant")
#'
#' @export
#'
#' @importFrom dplyr filter distinct pull
#' @importFrom rlang ensym as_string arg_match abort
rm_cases_string <- function(data,
                     column_rm,
                     factor_rm,
                     criteria = c("session", "participant"),
                     part_id = "mouseid",
                     sess_id = "workerId") {
  # ---- 1) Validate inputs (fast, clear errors) ----
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble.")
  }
  
  criteria <- rlang::arg_match(criteria)
  
  # ensym() lets users pass: column_rm = col   OR  column_rm = "col"
  column_rm <- rlang::as_string(rlang::ensym(column_rm))
  part_id   <- rlang::as_string(rlang::ensym(part_id))
  sess_id   <- rlang::as_string(rlang::ensym(sess_id))
  
  required_cols <- c(column_rm, part_id, sess_id)
  .validate_required_cols(data, required_cols)
  
  # ---- 2) Decide which ID column defines the removal unit ----
  id_col <- if (criteria == "session") sess_id else part_id
  
  # ---- 3) Find IDs to remove ----
  bad_ids <- data |>
    dplyr::filter(.data[[column_rm]] %in% factor_rm) |>
    dplyr::distinct(.data[[id_col]]) |>
    dplyr::pull(.data[[id_col]])
  
  # ---- 4) Remove those IDs ----
  out <- data |>
    dplyr::filter(!(.data[[id_col]] %in% bad_ids))
  
  out
}

#' Validate that required columns exist
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#' @noRd
.validate_required_cols <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    rlang::abort(paste0(
      "Missing required column(s): ",
      paste(missing, collapse = ", ")
    ))
  }
}
