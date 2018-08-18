#' Add the attacking direction to events 
#'
#' Adding a variable of 'attacking.direction' to the events dataframe allows us to keep
#' track of the direction of play and how that relates to x, y positioning. It's a basic
#' but crucial variable to add to our events dataframe to improve our analysis
#'
#' @param events An events dataframe created from the 'parse_f24' function 
#' @param tDAT A tracking dataframe created from the 'parse_tracking' function 
#' @param metaDF An tracking metadata dataframe created from the 'parse_CH_metadata' function 
#' @param GameCentre A list of dataframes created from the 'parse_f7' function 
#' @return An events dataframe with the added attacking.direction variable 
#' @export

add_attacking_direction <- function(events, tDAT, metaDF, GameCentre){

period1.GK.check <- tDAT %>% filter(FrameID == metaDF$period1Start + 100)
T1.GK <- GameCentre$PlayersDB %>% filter(team_HA == 1 & Position == "Goalkeeper") %>% select(JerseyNo)
period1.GK.position <- period1.GK.check %>% filter(team_HA == 1 & JerseyNo == as.numeric(T1.GK)) %>% select(x) %>% as.numeric()
attacking.direction.t1.p1 <- ifelse(period1.GK.position < 0, 1, -1)
attacking.direction.t2.p1 <- ifelse(attacking.direction.t1.p1 == 1, -1, 1)
attacking.direction.t1.p2 <- ifelse(attacking.direction.t1.p1 == 1, -1, 1)
attacking.direction.t2.p2 <- attacking.direction.t1.p1
  
attacking.direction.catch <- list()

for (i in 1:nrow(events)) {
  
  row.to.assess <- events[i,]
  team1.id <- GameCentre$PlayersDB %>% filter(team_HA == 1)
  team1.id <- team1.id$team_id
  
  ad_temp <- ""
  ad_temp <- ifelse(row.to.assess$period_id == 14, 2,attacking.direction.t2.p1)
  ad_temp <- ifelse(row.to.assess$period_id == 16, 3,attacking.direction.t2.p1)

  if(row.to.assess$period_id == 1){
    
              if(row.to.assess$team_id == team1.id){
                
              ad_temp <-attacking.direction.t1.p1
                
              }else{
              
              ad_temp <- attacking.direction.t2.p1    
                
              }
    
    
  }else{
    
              if(row.to.assess$team_id == team1.id){
              
              ad_temp <- attacking.direction.t1.p2

              }else{
              
              ad_temp <- attacking.direction.t2.p2

              }
    
  } # end of main 'if'

attacking.direction.catch <- append(attacking.direction.catch, ad_temp)    
  
  
} # end of for loop 


events$attacking.direction <- unlist(attacking.direction.catch) 
 return(events)
}
