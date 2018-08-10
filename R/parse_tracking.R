#' Parse an ChyronHego Tracking Data File 
#'
#' ChyronHego's tracking data contains positional data at 25 frames per second.
#' The raw data comes in .dat format with all players, officials and ball positional data
#' per frame combined in a string. This function spilts up the string and arranges the 
#' resulting data in a logical format. 
#'
#' @param tracking.xml Path to the input file
#' @param metadata Dataframe of metadata as created via parse_CH_metadata() in this package
#' @return A dataframe of the parsed tracking data 
#' @export

parse_tracking <- function(tracking.xml, metadata){

          print("// This process generally takes 45 minutes to process... go and make a cup of tea.......")

##//--- Define some utility functions -------------------------------------------------- //

    Unpack.1.Frame.of.Time <- function(frame.to.process){
          
          Parsing.Counter <<- 1
          temp.parts <- unlist(strsplit(as.character(frame.to.process), ":", fixed = FALSE, perl = FALSE, useBytes = FALSE))

          people.temp <- Unpack.1.Frame.of.Players(temp.parts[2])
          ball.temp <- Unpack.1.Frame.of.Ball(temp.parts[3])
          
          people.temp$Ball.Ownership <- ball.temp$Ball.Ownership
          people.temp$Ball.InPlay <- ball.temp$Ball.InPlay
          people.temp$Ball.Contact.Info1 <- ball.temp$Ball.Contact.Info1
          people.temp$Ball.Contact.Info2 <- ball.temp$Ball.Contact.Info2
          
          frame.temp <- bind_rows(people.temp, ball.temp)
          
          frame.temp$frameID <- temp.parts[1]

          if((Parsing.Counter/250)%%1==0){cat(".")}else{}
          Parsing.Counter <<- Parsing.Counter + 1

          return(frame.temp)
      
    }
    
    
    
    Unpack.1.Frame.of.Players <- function(df){
      
        unpack.catch <- unlist(strsplit(df, ";")) %>% 
        split(1:length(.)) %>% 
        purrr::map(Unpack.1.Player.of.Tracking) %>% 
        dplyr::bind_rows() 
    
        return(unpack.catch)
    }
    
    
    Unpack.1.Player.of.Tracking <- function(r){
        return(data.frame(team_HA = as.numeric(as.character(unlist(strsplit(unlist(r),","))[1])), 
                           TrackID = as.numeric(as.character(unlist(strsplit(unlist(r),","))[2])), 
                           JerseyNo = as.numeric(as.character(unlist(strsplit(unlist(r),","))[3])),
                           x = as.numeric(as.character(unlist(strsplit(unlist(r),","))[4])), 
                           y = as.numeric(as.character(unlist(strsplit(unlist(r),","))[5])),
                           speed = as.numeric(as.character(unlist(strsplit(unlist(r),","))[6])),
                           z = 10))
    }
    
    
    Unpack.1.Frame.of.Ball <- function(df){
    
         ball.parts <- unlist(strsplit(unlist(df),","))
         return(data.frame(team_HA = 10, 
                            TrackID = 50, 
                            JerseyNo = NA, 
                            x = as.numeric(as.character(ball.parts[1])), 
                            y = as.numeric(as.character(ball.parts[2])), 
                            z = as.numeric(as.character(ball.parts[3])),
                            speed = as.numeric(as.character(ball.parts[4])),
                            Ball.Ownership = ifelse(gsub(";","",ball.parts[5])=="A",0,1),
                            Ball.InPlay = ifelse(gsub(";","",ball.parts[6])=="Dead",0,1),
                            Ball.Contact.Info1 = ifelse(length(ball.parts[7])>0,ball.parts[7],NA),
                            Ball.Contact.Info2 = ifelse(length(ball.parts[8])>0,ball.parts[8],NA)))  
    }
    

##//--- Read Tracking Data ------------------------------------------------------------- //
  
    dat <- read.table(tracking.xml)
    dat <- dat$V1
    cat("// Dat file loaded....")
    
##//--- Prep Tracking Data ------------------------------------------------------------- //

    ## calculate the starting frame 
    starting.frame <- unlist(strsplit(as.character(dat[1]), ":", fixed = FALSE, perl = FALSE, useBytes = FALSE))[1]

    ## Trim off pre-game, half-time and post-game frames
    starting.index.p1 <- (metadata$period1Start - as.numeric(as.character(starting.frame))) - 1
    ending.index.p1 <- (metadata$period1End - as.numeric(as.character(starting.frame))) + 1

    starting.index.p2 <- (metadata$period2Start - as.numeric(as.character(starting.frame))) - 1
    ending.index.p2 <- (metadata$period2End - as.numeric(as.character(starting.frame))) + 1

    starting.index.p3 <- (metadata$period3Start - as.numeric(as.character(starting.frame))) - 1
    ending.index.p3 <- (metadata$period3End - as.numeric(as.character(starting.frame))) + 1

    starting.index.p4 <- (metadata$period4Start - as.numeric(as.character(starting.frame))) - 1
    ending.index.p4 <- (metadata$period4End - as.numeric(as.character(starting.frame))) + 1

    indexes.to.select <- if(metadata$period4Start > 0){
      c(starting.index.p1:ending.index.p1, starting.index.p2:ending.index.p2, starting.index.p3:ending.index.p3, starting.index.p4:ending.index.p4)}else{
      c(starting.index.p1:ending.index.p1, starting.index.p2:ending.index.p2)}

    dat <- dat[indexes.to.select] ## new trimmed data before we spilt it and deal with it... saves a lot of time!
    cat("// Dat file trimmed....")

    
##//--- Process Tracking Data --------------------------------------------------------- //

    dat.chunks <- split(dat, ceiling(seq_along(dat)/(length(dat)/10)))
    
    cat("//Unpacking starting....")
    
    tDAT1 <-  dat.chunks$`1` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 1 of 10 complete.... ")  
    
    tDAT2 <-  dat.chunks$`2` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 2 of 10 complete.... ")  

    tDAT3 <-  dat.chunks$`3` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 3 of 10 complete.... ")  

    tDAT4 <-  dat.chunks$`4` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 4 of 10 complete.... ")  

    tDAT5 <-  dat.chunks$`5` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 5 of 10 complete.... ")  
        
    tDAT6 <-  dat.chunks$`6` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()    
    cat("//Section 6 of 10 complete.... ")  
    
    tDAT7 <-  dat.chunks$`7` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 7 of 10 complete.... ")  
      
    tDAT8 <-  dat.chunks$`8` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 8 of 10 complete.... ")  
        
    tDAT9 <-  dat.chunks$`9` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()
    cat("//Section 9 of 10 complete.... ")  
    
    tDAT10 <-  dat.chunks$`10` %>% 
    split(1:length(.)) %>% 
    purrr::map(Unpack.1.Frame.of.Time) %>% 
    dplyr::bind_rows()    
    Unpack.1.Frame.of.Time(dat[1])
    cat("//Section 10 of 10 complete.... ")  
    
##//--- Combine and Return Results --------------------------------------------------------- //

    tDAT <- bind_rows(list(tDAT1, tDAT2, tDAT3, tDAT4, tDAT5, tDAT6, tDAT7, tDAT8, tDAT9, tDAT10))
    cat(paste0("//Tracking data parse complete with ", nrow(tDAT), " observations parse"))

    return(tDAT)
    
} # end fo parse_tracking function 
    
    