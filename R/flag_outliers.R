#' Flag move time outliers
#'
#' Flags outlier values in a numeric column using one of two criteria:
#' a data-based rule or a fixed threshold. The function can also return the
#' data unchanged except for an added outlier flag when no detection criterion
#' is selected.
#'
#' @param data A data frame or tibble.
#' @param flag_outliers A character string specifying the outlier detection
#'   method. Must be one of `"none"`, `"data"`, or `"threshold"`.
#'   Defaults to `"none"`.
#' @param filter_var A character string naming the numeric column on which
#'   outlier detection should be performed. Defaults to `"move_time"`.
#' @param outlier_threshold A numeric threshold used when
#'   `flag_outliers = "threshold"`. Values above this threshold are flagged
#'   as outliers. Defaults to `120000`.
#'
#' @details
#' If `flag_outliers = "data"`, values are flagged as outliers when they are
#' greater than the mean plus two standard deviations of `filter_var`.
#'
#' If `flag_outliers = "threshold"`, values are flagged as outliers when they
#' are greater than `outlier_threshold`.
#'
#' If `flag_outliers = "none"`, no values are flagged and the output column
#' `flag_outlier` is set to `FALSE` for all rows.
#'
#' @return A data frame with the same rows as the input and an additional
#'   logical column, `flag_outlier`, indicating whether each row was flagged
#'   as an outlier.
#'
#' @examples
#' # Flag outliers using the data-based rule
#' # time_data <- flag_outliers(df, flag_outliers = "data")
#'
#' # Flag outliers using a fixed threshold
#' # time_data <- flag_outliers(df, flag_outliers = "threshold",
#' #                            outlier_threshold = 120000)
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang abort
flag_outliers <- function(data, 
                          flag_outliers = "none", 
                          filter_var = "move_time",
                          outlier_threshold = 12e4){

    
  
  if (!(flag_outliers %in% c("none", "data", "threshold"))) {
    rlang::abort("Check specified `flag_outliers` criterion.")
  }
  
  if (!filter_var %in% names(data)) {
    rlang::abort(paste0("Column `", filter_var, "` not found in `data`."))
  }
    
    var <- data[[filter_var]]
  
  flag_outlier <- rep(FALSE, nrow(data))
  
  if (flag_outliers=="data"){
    flag_outlier <- ifelse(var > mean(var, na.rm = TRUE) + 2 * sd(var, na.rm = TRUE),TRUE,FALSE)
  }
  
  
  if (flag_outliers=="threshold"){
    flag_outlier = ifelse(var > outlier_threshold,TRUE,FALSE)
  }
  
  return(data %>% mutate(flag_outlier = flag_outlier))
}