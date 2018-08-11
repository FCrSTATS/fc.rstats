#' Convert the pass destination from OPTA to ChyronHego spec 
#'
#' OPTA event data has two variables 140 and 141, that represent the the x and y pitch
#' coordinates for the end point of a pass. We can convert from OPTA 0-100 values to 
#' ChyronHego CM and return the events dataframe back with new variables of target.x and 
#' target.y. The attacking.direction needs to be added to the events dataframe first from 
#' the 'add_attacking_direction' function in this package. 
#'
#' @param df An events dataframe created from the 'parse_event' function 
#' @return An events dataframe with the added target.x and target.y variables
#' @export

add_pass_target <- function(df){
  
  target.x.catch <- list()
  target.y.catch <- list() 

  for (i in 1:nrow(df)) {
          
        pass <- df[i,]
        
        pass$`140` <- as.numeric(as.character(pass$`140`))
        pass$`141` <- as.numeric(as.character(pass$`141`))
        
        if(pass$type_id !=1 & pass$outcome !=1){
          target.x <- NA
          target.y <- NA
          target.x.catch <- append(unlist(target.x.catch), target.x)
          target.y.catch <- append(unlist(target.y.catch), target.y)
        }else{
        ## calculate the pitch translation for x
        target.x <- if(pass$`140` == 50){
          0
        }else{
          if(pass$`140` > 50){
            (((pass$`140`-50)*2)/100) * 5250
          }else{
            (((50-pass$`140`)*2)/100) * -5250
          } # end of n.if
        } # end of i.if
        
          ## calculate the pitch translation for y
        target.y <- if(pass$`141` == 50){
          0
        }else{
          if(pass$`141` > 50){
            (((pass$`141`-50)*2)/100) * 3300
          }else{
            (((50-pass$`141`)*2)/100) * -3300
          } # end of n.if
        } # end of i.id
        
        target.x <- ifelse(pass$attacking.direction == -1, -target.x, target.x)
        target.y <- ifelse(pass$attacking.direction == -1, -target.y, target.y)
        
        target.x.catch <- append(unlist(target.x.catch), target.x)
        target.y.catch <- append(unlist(target.y.catch), target.y)
  } # end of else 
  } # end of for loop 

  df$target.x <- target.x.catch
  df$target.y <- target.y.catch
  
  return(df)  
}
