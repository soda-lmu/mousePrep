#' Export mousetrap data in long or wide format
#'
#' Exports a mousetrap object to long or wide format using
#' `mt_export_long()` or `mt_export_wide()`. Optionally joins the exported
#' data with an additional data frame. For wide exports, summary measures can
#' also be included when available.
#'
#' @param data A mousetrap object or compatible object containing trajectory
#'   data for export.
#' @param direction A character string specifying the export format.
#'   Must be either `"long"` or `"wide"`.
#' @param incl_measures_wide A logical value indicating whether a measures
#'   object should be included in wide exports. Defaults to `FALSE`.
#' @param long_join_data An optional data frame to join to the long-format
#'   export. Only used when `direction = "long"`. Defaults to `NULL`.
#' @param wide_join_data An optional data frame to join to the wide-format
#'   export. Only used when `direction = "wide"`. Defaults to `NULL`.
#' @param id A single character string naming the join key column present in
#'   both exported data and the join data. Defaults to `"mt_id"`.
#'
#' @return A data frame containing the exported mousetrap data in the requested
#'   format, optionally joined with additional data.
#'
#' @details
#' When `incl_measures_wide = TRUE` for wide exports, the function adds a `"measures"` object. 
#' If multiple such objects are found, the first one is used and a warning is issued.
#' Joins are performed with `dplyr::inner_join()`, so only rows with matching
#' keys in both inputs are retained. 
#'
#' @examples
#' 
#' # Long export
#' long_data <- mp_export_data(data = mt_obj, direction = "long")
#'
#' # Wide export with measures
#' wide_data <- mp_export_data(
#'   data = mt_data,
#'   direction = "wide",
#'   incl_measures_wide = TRUE
#' )
#'
#' # Long export with join
#' long_joined <- mp_export_data(
#'   data = mt_data,
#'   direction = "long",
#'   long_join_data = participant_info,
#'   id = "mt_id"
#' )
#' 
#' @export
#'
#' @importFrom mousetrap mt_export_long mt_export_wide
#' @importFrom dplyr inner_join
#' @importFrom rlang abort warn


mp_export_data <- function(data, direction, 
                           incl_measures_wide = FALSE, 
                           long_join_data = NULL, 
                           wide_join_data = NULL, 
                           id = "mt_id") {
  
  if (direction != "wide" && direction != "long"){
    rlang::abort("`direction` must be either 'long' or 'wide'.")
  }
  
  if (direction == "long" && !is.null(wide_join_data)) {
    rlang::abort("`wide_join_data` can only be used when `direction = 'wide'`.")
  }
  
  if (direction == "wide" && !is.null(long_join_data)) {
    rlang::abort("`long_join_data` can only be used when `direction = 'long'`.")
  }
  
  measure_names <- names(data)[grepl("measures", names(data))]
  
  export_data <- NULL
  
  if (direction == "long") {
    export_data <- mt_export_long(data)
    
    if (!is.null(long_join_data)) {
      export_data <- dplyr::inner_join(export_data, long_join_data, by = id)
    }
  }
  
  
  if (direction == "wide"){
    if (incl_measures_wide == TRUE){
      
      measure_name <- measure_names[1]
      if (length(measure_names == 0)){
        rlang::abort("`incl_measures_wide = 'yes'` but no measures object was found in `data`.")
      }
      if (length(measure_names > 1)){
        rlang::warn("Multiple measure objects in `data`. Choosing the first one")
      }
      
      export_data <- mt_export_wide(data,
                     use2 = measure_name,
                     use2_variables = colnames(data[[measure_name]])) 
    } else {export_data <- mt_export_wide(data)}
    
    if (!is.null(wide_join_data)){
      export_data <- inner_join(export_data, wide_join_data, by = id)
    }
  }
  
  return(export_data)
 
  
}




