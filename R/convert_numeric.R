#' Convert numeric-like columns to numeric
#'
#' Detects columns whose non-missing, non-empty values can be interpreted as
#' numeric and converts them to numeric format. Before conversion, decimal
#' commas are replaced with decimal points.
#'
#' Columns can be excluded from conversion using the `exclude` argument.
#'
#' @param data A data frame or tibble.
#' @param exclude An optional character vector of column names to exclude from
#'   conversion. Defaults to `NULL`.
#'
#' @return A data frame with numeric-like columns converted to numeric.
#'
#' @details
#' A column is treated as numeric-like if all of its non-missing and non-empty
#' values can be converted to numeric after replacing `","` with `"."`.
#' This is useful for datasets where numeric values are stored as character
#' strings, including those using decimal commas.
#' 
#' df <- convert_numeric(df, exclude = "id")
#'
#' @export
#' @importFrom dplyr mutate across all_of
convert_numeric <- function(data, exclude = "mt_id"){
  
  is_numeric_like <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[x != "" & !is.na(x)]
    
    if (length(x) == 0) {
      return(FALSE)
    }
    
    x_clean <- sub(",", ".", x, fixed = TRUE)
    
    suppressWarnings(all(!is.na(as.numeric(x_clean))))
  }
  
  cols_to_convert <- names(data)[vapply(data, is_numeric_like, logical(1))]
  
  if (!is.null(exclude)) {
    cols_to_convert <- setdiff(cols_to_convert, exclude)
  }
  
  
  data <- data %>%
    mutate(across(all_of(cols_to_convert),
                  ~ as.numeric(sub(",", ".", as.character(.x), fixed = TRUE))))
  
  
  data
}


