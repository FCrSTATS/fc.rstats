#' Add a TimeSync variable 
#'
#' The OPTA and ChyronHego datasets don't share a common clock. OPTA data is recorded in seconds and ChyronHego
#' data is recorded at 25 frames a second. The addition of a TimeSync variable to the events dataframe allows 
#' the two dataframes to be synced and linked together easier with a common clock.
#'
#' @param events An events dataframe created from the 'parse_f24' function 
#' @param metaDF An tracking metadata dataframe created from the 'parse_CH_metadata' function 
#' @return An events dataframe with the added TimeSync variable 
#' @export


add_TimeSync <- function(events, metaDF){
  
  TimeSync <- list()
  
  for (n in 1:nrow(events)) {
      
      TimeSync.value <- 0
      timer <- 0
      row <- events[n,]
    
      if(row$period_id == 1){
        
      timer <- (row$min * 60)+ row$sec
      TimeSync.value <- (timer * 25) + metaDF$period1Start
      
      }
  
     if(row$period_id == 2){
    
      timer <- ((row$min-45) * 60)+ row$sec
      TimeSync.value <- (timer * 25) + metaDF$period2Start
      
      }  
    
      if(row$period_id == 3){
    
      timer <- ((row$min-90) * 60)+ row$sec
      TimeSync.value <- (timer * 25) + metaDF$period3Start
      
      }  
      
      if(row$period_id == 4){
    
      timer <- ((row$min-105) * 60)+ row$sec
      TimeSync.value <- (timer * 25) + metaDF$period4Start
      
       }  
  
  TimeSync <- append(TimeSync, TimeSync.value)   
  } ## end of for loop 
  
  return(unlist(TimeSync))
  
}  # end of function 
