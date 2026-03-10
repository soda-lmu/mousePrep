#' Calculate effective screen dimensions (general + UAS mode)
#'
#' Computes row-wise "effective" width and height as the maximum of:
#' width: max(inner_width, scroll_width) and height: max(inner_height, scroll_height).
#'
#' If `uas = TRUE`, the function uses UAS default column names and defaults for
#' filtering/keeping columns, so you can call it with only `data`.
#'
#' @param data A data frame / tibble.
#' @param uas Logical. If TRUE, use UAS defaults for columns and filtering/keeping.
#' @param inner_width,inner_height,scroll_width,scroll_height Column specs (string or unquoted).
#' @param filter_nonblank_col Optional column used to filter nonblank rows (string or unquoted).
#' @param keep_cols Optional character vector of columns to keep before computing.
#' @param out_width,out_height Output column names.
#'
#' @return Dataset with computed width/height columns.
#'
#' @export
#' @importFrom dplyr mutate across all_of filter select
#' @importFrom rlang abort
screen_dimension_calculator <- function(
    data,
    uas = TRUE,
    inner_width = NULL,
    inner_height = NULL,
    scroll_width = NULL,
    scroll_height = NULL,
    filter_nonblank_col = NULL,
    keep_cols = NULL,
    out_width = "screen_width",
    out_height = "screen_height"
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble.")
  }
  
  #Apply UAS defaults if requested
  if (isTRUE(uas)) {
    if (is.null(inner_width))  inner_width  <- "inner_width"
    if (is.null(inner_height)) inner_height <- "inner_height"
    if (is.null(scroll_width)) scroll_width <- "scroll_width"
    if (is.null(scroll_height)) scroll_height <- "scroll_height"
    if (is.null(filter_nonblank_col)) filter_nonblank_col <- "screen_width"
    if (is.null(keep_cols)) {
      keep_cols <- c(
        "mt_id", "screen_width", "screen_height",
        "inner_width", "inner_height", "scroll_width", "scroll_height"
      )
    }
  }
  
  # Require the 4 core columns
  if (is.null(inner_width) || is.null(inner_height) ||
      is.null(scroll_width) || is.null(scroll_height)) {
    rlang::abort(
      "You must provide inner_width, inner_height, scroll_width, and scroll_height (or set uas = TRUE)."
    )
  }
  
  # Resolve column specs safely (strings or bare names)
  inner_width   <- .as_colname(inner_width)
  inner_height  <- .as_colname(inner_height)
  scroll_width  <- .as_colname(scroll_width)
  scroll_height <- .as_colname(scroll_height)
  if (!is.null(filter_nonblank_col)) filter_nonblank_col <- .as_colname(filter_nonblank_col)
  
  required <- c(inner_width, inner_height, scroll_width, scroll_height)
  if (!is.null(filter_nonblank_col)) required <- c(required, filter_nonblank_col)
  
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  out <- data
  
  # Filter non blank column
  if (!is.null(filter_nonblank_col)) {
    out <- out |>
      dplyr::filter(!is.na(.data[[filter_nonblank_col]]),
                    .data[[filter_nonblank_col]] != "")
  }
  
  # Select required columns before converting them to numeric
  if (!is.null(keep_cols)) {
    if (!is.character(keep_cols)) {
      rlang::abort("`keep_cols` must be a character vector of column names or NULL.")
    }
    keep_missing <- setdiff(keep_cols, names(out))
    if (length(keep_missing) > 0) {
      rlang::abort(paste0(
        "Requested `keep_cols` not found in data: ",
        paste(keep_missing, collapse = ", ")
      ))
    }
    out <- out |> dplyr::select(dplyr::all_of(keep_cols))
  }
  
  dim_cols <- c(inner_width, inner_height, scroll_width, scroll_height)
  
  # Robust numeric conversion
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(dim_cols),
        ~ {
          if (is.character(.x)) {
            .x <- trimws(.x)
            .x[.x == ""] <- NA_character_
          }
          as.numeric(.x)
        }
      )
    )
  
  # Vectorized row-wise maxima
  w <- pmax(out[[inner_width]],  out[[scroll_width]],  na.rm = TRUE)
  h <- pmax(out[[inner_height]], out[[scroll_height]], na.rm = TRUE)
  w[is.infinite(w)] <- NA_real_
  h[is.infinite(h)] <- NA_real_
  
  out[[out_width]]  <- w
  out[[out_height]] <- h
  out
}
