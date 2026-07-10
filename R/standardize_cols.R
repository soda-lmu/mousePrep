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
#' @param position A character string specifying the columns to be renamed to xpos and ypos. The default is `page`
#'
#' @return `data` with renamed columns.
#'
#' @examples
#' # df2 <- standardize_colnames(df, verbose = TRUE)
#'
#' @export
#' @importFrom dplyr rename_with setdiff
#' @importFrom rlang inform warn

standardize_colnames <- function(data, verbose = TRUE, position = "page"){
  
  renamed_data <- data
  
  stand_col <- function(colname, to) {
    rep(to, length(colname))
  }
  

  rules <- tibble::tibble(
    pattern = c(
      "(?i)^(mt_id|.*worker.*id.*)$",
      "(?i)^(timestamps|.*time.*stamp.*)$",
      "(?i)^(paradatasession|.*para.*data.*session.*)$",
      "(?i)^(dataset|.*data.*set.*)$",
      "(?i)^(screen_width|.*screen.*width.*)$",
      "(?i)^(screen_height|.*screen.*height.*)$",
      "(?i)^(inner_width|.*inner.*width.*)$",
      "(?i)^(inner_height|.*inner.*height.*)$",
      "(?i)^(scroll_width|.*scroll.*width.*)$",
      "(?i)^(scroll_height|.*scroll.*height.*)$",
      "(?i)^(xpos|.*page.*x.*)$",
      "(?i)^(xpos|.*client.*x.*)$",
      "(?i)^(ypos|.*page.*y.*)$",
      "(?i)^(ypos|.*client.*y.*)$"
    ),
    to = c(
      "mt_id",
      "timestamps",
      "paradatasession",
      "dataset",
      "screen_width",
      "screen_height",
      "inner_width",
      "inner_height",
      "scroll_width",
      "scroll_height",
      "xpos",
      "xpos",
      "ypos",
      "ypos"
    )
  )
  
  col_matches <- logical(0)
  
  for (i in seq_len(nrow(rules))){
    rule_match <- grepl(rules$pattern[i], colnames(renamed_data), perl = TRUE)
    
    if (all(!rule_match)) {
      col_matches[i] <- FALSE
    } else {
      col_matches[i] <- TRUE
    }
  }
  
  if(all(!col_matches)){
    rlang::warn("No columns match the rule") 
  }
  
  if(position=="page"){
    rules = rules[!grepl("client", rules$pattern, ignore.case = TRUE), ]
  }else {rules = rules[!grepl("page", rules$pattern, ignore.case = TRUE), ]}
    
  
  
  for (i in seq_len(nrow(rules))){
    renamed_data <- rename_with(
      renamed_data,
      stand_col,
      matches(rules$pattern[i]),
      to = rules$to[i]
    )
  }
  
  if (verbose == TRUE){
    renamed_cols <- setdiff(names(renamed_data), names(data))
    rlang::inform("Renamed columns:") 
    print(renamed_cols)
  }
  
  renamed_data
}

  
 
  
  
 