#' Calculate effective screen dimensions 
#'
#' Computes row-wise "effective" screen width and height as the maximum of:
#' screen width: max(inner_width, scroll_width) and screen height: max(inner_height, scroll_height).
#'
#' If `uas = TRUE`, the function uses UAS default column names and defaults for
#' filtering columns, so you can call the function with only `data`.
#'
#' @param data Any mouse trajectory dataset that contains inner and scroll dimensions of the screen.
#' @param uas Logical. If TRUE, use UAS defaults for columns and filtering.
#' @param inner_width,inner_height,scroll_width,scroll_height Column specs (string). 
#' Inner dimensions correspond to the screen visible in the browser while scroll dimensions are of the physical monitor.
#' @param screen_width,screen_height Screen height and screen width. If it exists in the data and wish to 
#' replace with the calculation, input the variable names.
#'
#' @return Dataset with (re)computed screen width and screen height columns.
#' 
#' @examples
#' # screen_dims_data <- calculate_screen_dims(df) 
#'
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom rlang abort
calculate_screen_dims <- function(data, 
                             uas = TRUE, 
                             inner_height = NULL, 
                             inner_width = NULL, 
                             scroll_height = NULL, 
                             scroll_width = NULL, 
                             screen_height = NULL, 
                             screen_width = NULL ){
  
  if (isTRUE(uas)) {
    if (is.null(inner_width))  inner_width  <- "inner_width"
    if (is.null(inner_height)) inner_height <- "inner_height"
    if (is.null(scroll_width)) scroll_width <- "scroll_width"
    if (is.null(scroll_height)) scroll_height <- "scroll_height"
    if (is.null(screen_width)) screen_width <- "screen_width"
    if (is.null(screen_height)) screen_height <- "screen_height"
  }
  
  
  
  required <- c(inner_width, inner_height, scroll_width, scroll_height, screen_height, screen_width)
  
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  dimension_cols <- c(inner_width,inner_height,scroll_width,scroll_height)
  
  data <- data %>%
    filter(if_any(all_of(dimension_cols),~ !is.na(.x) & .x != "")) %>%
    mutate(
      !!screen_height := pmax(.data[[inner_height]], .data[[scroll_height]], na.rm = TRUE),
      !!screen_width  := pmax(.data[[inner_width]], .data[[scroll_width]], na.rm = TRUE))
    
    
  data
  
}
