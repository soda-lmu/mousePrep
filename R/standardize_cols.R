# R/rename_standardize_cols.R

#' Standardize common column names to package conventions
#'
#' This function renames frequently used variables to a standard naming scheme for mouse processing.
#' Rule examples (case-insensitive):
#' \itemize{
#'   \item any name containing "worker" and "id"  -> "mt_id"
#'   \item any name containing "time" and "stamp" -> "time_stamp"
#'   
#' }
#'
#' The function ican be extended to add more rules to the `rules` table later.
#'
#' @param data A data frame / tibble.
#' @param verbose Logical. If TRUE, prints a small summary of renames. The default is TRUE.
#'
#' @return `data` with renamed columns.
#'
#' @examples
#' # df2 <- standardize_colnames(df, verbose = TRUE)
#'
#' @export
#' @importFrom dplyr rename_with setdiff
#' @importFrom rlang inform warn
standardize_colnames <- function(data, verbose = TRUE){
  
  
  renamed_data <- data
  stand_col <- function(colname, to) {
    rep(to, length(colname))
  }
  
  rules <- list(
    list(pattern = "(?i)^(mt_id|.*worker.*id.*)$", to = "mt_id"),
    list(pattern = "(?i)^(timestamps|.*time.*stamp.*)$", to = "timestamps"),
    list(pattern = "(?i)^(paradatasession|.*para.*data.*session.*)$", to = "paradatasession"),
    list(pattern = "(?i)^(dataset|.*data.*set.*)$", to = "dataset"),
    list(pattern = "(?i)^(screen_width|.*screen.*width.*)$", to = "screen_width"),
    list(pattern = "(?i)^(screen_height|.*screen.*height.*)$", to = "screen_height"),
    list(pattern = "(?i)^(inner_width|.*inner.*width.*)$", to = "inner_width"),
    list(pattern = "(?i)^(inner_height|.*inner.*height.*)$", to = "inner_height"),
    list(pattern = "(?i)^(scroll_width|.*scroll.*width.*)$", to = "scroll_width"),
    list(pattern = "(?i)^(scroll_height|.*scroll.*height.*)$", to = "scroll_height")
  )
  
  col_matches <- logical(0)
  for (i in seq_along(rules)){
    rule_match <- grepl(rules[[i]]$pattern, colnames(renamed_data), perl = TRUE)
    if (all(!rule_match)) {
      col_matches[i] <- FALSE
    } else{
      col_matches[i] <- TRUE
    }
  }
  if(all(!col_matches)){
    rlang::warn("No columns match the rule") 
  }
  
  
  
  for (i in seq_along(rules)){
    renamed_data <- rename_with(renamed_data, stand_col, matches(rules[[i]]$pattern), to = rules[[i]]$to)
  }
   
  if (verbose == TRUE){
    renamed_cols <- setdiff(names(renamed_data), names(data))
    rlang::inform("Renamed columns:") 
    print(renamed_cols)
  }
  
  
  renamed_data
}


