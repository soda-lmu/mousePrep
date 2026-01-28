# R/rename_standardize_cols.R

#' Standardize common column names to package conventions
#'
#' Renames frequently used variables to a standard naming scheme.
#' Current rules (case-insensitive):
#' \itemize{
#'   \item any name containing "worker" and "id"  -> "mt_id"
#'   \item any name containing "time" and "stamp" -> "time_stamp"
#'   \item any name containing "data" and "session" -> "data_session"
#' }
#'
#' The function is designed to be extended: add more rules to the internal
#' `rules` table later.
#'
#' @param data A data frame / tibble.
#' @param verbose Logical. If TRUE, prints a small summary of renames.
#'
#' @return `data` with renamed columns.
#'
#' @examples
#' # df2 <- standardize_colnames(df)
#'
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang abort
standardize_colnames <- function(data, verbose = FALSE) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble.")
  }
  
  old_names <- names(data)
  
  # ---- Rules: add more later ----
  # Each rule is a regex pattern (applied to the full column name) and a target name.
  rules <- list(
    list(pattern = "(?i).*worker.*id.*",      to = "mt_id"),
    list(pattern = "(?i).*time.*stamp.*",    to = "timestamps"),
    list(pattern = "(?i).*para. *data.*session.*",  to = "paradatasession"),
    list(pattern = "(?i).*data.*set.*", to = "dataset"),
    list(pattern = "(?i).*screen.*width", to = "screen_width"),
    list(pattern = "(?i).*screen.*height", to = "screen_height"),
    list(pattern = "(?i).*inner.*width", to = "inner_width"),
    list(pattern = "(?i).*inner.*height", to = "inner_height"),
    list(pattern = "(?i).*scroll.*width", to = "scroll_width"),
    list(pattern = "(?i).*scroll.*height", to = "scroll_height")
  )
  
  new_names <- old_names
  
  # Apply rules in order: first match wins (prevents later rules overriding earlier ones)
  for (i in seq_along(old_names)) {
    nm <- old_names[i]
    for (r in rules) {
      if (grepl(r$pattern, nm, perl = TRUE)) {
        new_names[i] <- r$to
        break
      }
    }
  }
  
  # If nothing changes, return early
  if (identical(old_names, new_names)) return(data)
  
  # ---- Collision guard ----
  # If multiple columns map to the same standardized name, renaming would create duplicates.
  dups <- unique(new_names[duplicated(new_names)])
  if (length(dups) > 0) {
    # show which originals collide
    collision_map <- lapply(dups, function(d) old_names[new_names == d])
    names(collision_map) <- dups
    
    msg <- paste0(
      "Renaming would create duplicate column name(s): ",
      paste(dups, collapse = ", "),
      "\nConflicts:\n",
      paste(
        vapply(names(collision_map), function(d) {
          paste0("  ", d, " <- ", paste(collision_map[[d]], collapse = ", "))
        }, character(1)),
        collapse = "\n"
      ),
      "\n\nFix by renaming one of the conflicting columns first, or extend the rules "
    )
    
    rlang::abort(msg)
  }
  
  # ---- Rename by setting names (fast, preserves tibble/data.frame) ----
  names(data) <- new_names
  
  if (isTRUE(verbose)) {
    changed <- which(old_names != new_names)
    summary <- paste0("Renamed ", length(changed), " column(s):\n",
                      paste0("  ", old_names[changed], " -> ", new_names[changed], collapse = "\n"))
    message(summary)
  }
  
  data
}
