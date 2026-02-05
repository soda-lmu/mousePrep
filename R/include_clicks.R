#' Include clicks to the preprocessed dataset 
#'
#' This function takes the data, timestamps, initiation time, and click times, and adds a column indicating clicks using one-hot encoding.
#' Clicks are aligned to the closest available timestamp. As a result, the original click data frame and the resulting data frame may differ in row count.
#' 
#'
#' @param data_complete A data frame with the screen ID, timestamps, and repeated initiation time
#' @param data_clicks A data frame with the screen ID and timestamps of the clicks 
#' @param screen_id The screen or worker ID, identifies the separated screens, one participant has multiple IDs. The screen ID should be identical for both data frames. 
#' @param click_column The click column. 
#' @param timestamps The timestamp column. 
#' @param initiation_time  The initiation time column, with the initiation time repeated for each screen.
#' 
#' @return The original data frame with an additional click column. 
#' 
#' 
#' @examples
#' click_data <- include_clicks(df_complete, df_click, click_column = "timestamps")
#' @export

include_clicks  <- function(data_complete, data_clicks, screen_id = "mt_id", click_column = "timestamps", timestamps = "timestamps", initiation_time = "initiation_time") {
  
  #transform click variable
  clicks_char <- by(data_clicks[[click_column]], data_clicks[[screen_id]], function(x){
    paste0(x, collapse = '_')
  })
  
  clicks_char <- data.frame(names = names(clicks_char), clicks_char)
  names(clicks_char)[1] <- screen_id
  
  # perform an left join
  clicks_char <- clicks_char[clicks_char[[screen_id]] %in% data_complete[[screen_id]], ]
  full_df <- merge(data_complete, clicks_char, by = screen_id, all.x = TRUE)
  
  full_df <- as.data.frame(full_df)
  
 
  #one-hot encoding of the click variable
  click_indicator <- by(full_df[which(full_df$clicks_char!=''), c('timestamps', 'clicks_char', 'initiation_time')], full_df[[screen_id]][which(full_df$clicks_char!='')],
                        function(x){
                          rowSums(matrix(diag(nrow(x))[,by(abs(expand.grid(x[,1]+x[,3], as.numeric(strsplit(as.character(x[1,2]), '_')[[1]]))[,1] -
                                                                 expand.grid(x[,1]+x[,3], as.numeric(strsplit(as.character(x[1,2]), '_')[[1]]))[,2]),
                                                           rep(1:length(strsplit(as.character(x[1,2]), '_')[[1]]), each = nrow(x)),
                                                           function(y){
                                                             which.min(y)
                                                           })], nrow = nrow(x)))})

  full_df$click <- 0
  full_df$click[which(full_df$clicks_char!='')] <- unlist(click_indicator)

  return(full_df)
  
}





