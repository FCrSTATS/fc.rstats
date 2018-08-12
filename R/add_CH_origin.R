#' Convert the pass origin from OPTA to ChyronHego spec 
#'
#' OPTA event data has two variables x and y, that represent the the x and y pitch
#' coordinates for the origina point of an event. We can convert from OPTA 0-100 values to 
#' ChyronHego CM and return the events dataframe back with new variables of origin.x and 
#' origin.y. The attacking.direction needs to be added to the events dataframe first from 
#' the 'add_attacking_direction' function in this package. 
#'
#' @param df An events dataframe created from the 'parse_event' function 
#' @return An events dataframe with the added origin.x and origin.y variables
#' @export

add_CH_origin <- function(df){
  
  origin.x.catch <- list()
  origin.y.catch <- list() 
  
  for (i in 1:nrow(df)) {
          
        pass <- df[i,]
        
        pass$x
        pass$y 
        
        ## calculate the pitch translation for x
        origin.x <- if(pass$x == 50){
          0
        }else{
          if(pass$x > 50){
            (((pass$x-50)*2)/100) * 5250
          }else{
            (((50-pass$x)*2)/100) * -5250
          } # end of n.if
        } # end of i.if
        
          ## calculate the pitch translation for y
        origin.y <- if(pass$y == 50){
          0
        }else{
          if(pass$y > 50){
            (((pass$y-50)*2)/100) * 3300
          }else{
            (((50-pass$y)*2)/100) * -3300
          } # end of n.if
        } # end of i.if
        
        origin.x <- ifelse(pass$attacking.direction == -1, -origin.x, origin.x)
        origin.y <- ifelse(pass$attacking.direction == -1, -origin.y, origin.y)

        origin.x.catch <- append(unlist(origin.x.catch), origin.x)
        origin.y.catch <- append(unlist(origin.y.catch), origin.y)
        
  } # end of for loop 

  df$origin.x <- origin.x.catch
  df$origin.y <- origin.y.catch
  
  return(df)  
}