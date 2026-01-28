#' Coerce a column specification to a column name
#'
#' Accepts either a string ("col") or an unquoted name (col).
#' @noRd
.as_colname <- function(x) {
  if (is.character(x) && length(x) == 1) return(x)
  rlang::as_name(rlang::ensym(x))
}
