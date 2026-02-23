#' Remove participants or screens from a dataset
#'
#' This function removes participants or screens from a dataset based on their ID. 
#'
#'
#' @param data A data frame containing mouse movement data.
#' @param part_id Column name of the participant or screen identifier.
#' @param rm_vector A vector of all IDs that need to be removed. 
#'
#' @return A filtered data frame with the specified participants or screens removed.
#' @examples
#' new_df <- rm_part(df, "mt_id", part_vec)
#' 
#' @export


rm_part <- function(data, part_id = "mt_id", rm_vector) {

    new_data <- data[which(!(data[[part_id]] %in% rm_vector)),]
    
    return(new_data)
}
