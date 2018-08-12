#' Parse an OPTA f7 Files 
#'
#' An OPTA f7 file is an xml file with various information about a match such as 
#' lineups, goals, cards, kits used and more. This function takes the file location 
#' and parses the XML file into a dataframes which are stored in a list. 
#'
#' @param f7.xml Path to the input file
#' @param MatchID The ID number of the game to be parsed
#' @return A list of dataframes containing match information
#' @export

parse_f7 <- function(f7.xml, MatchID){
   
#//------ Load XML --------------------------------------------------------//
  
    pbpParse <- xmlInternalTreeParse(f7.xml)
    
#//------ Player DB ------------------------------------------------------//
    
    Players <- xpathSApply(pbpParse, paste("//", "Team/Player", "[@*]", sep=""), xmlValue)
    Players <- t(Players)
    
    ## establish the total playing time 
    tempx <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "MatchData/Stat", sep=""), xmlValue)) , stringsAsFactors = F)
    colnames(tempx) <- xpathSApply(pbpParse, paste("//", "MatchData/Stat", "[@*]", sep=""), xmlAttrs)
    TotalMatchTime <- as.numeric(as.character(tempx$match_time))
    
    lineup.catch <- data.frame(Formation_Place = character(), PlayerRef = character(), Position = character(), ShirtNumber = character(), Status = character(), stringsAsFactors = F)
    lineup <- xpathSApply(pbpParse, paste("//", "MatchPlayer", "[@*]", sep=""), xmlAttrs)
    
    for (l in 1:length(lineup)) {
      temp <- as.data.frame(t(unlist(lineup[l])), stringsAsFactors = FALSE)
      temp$PlayerRef <- ifelse(length(temp$PlayerRef) > 0, temp$PlayerRef, temp$SubPosition)
      temp <- if(NCOL(temp) >5){temp[c("Formation_Place", "PlayerRef", "Position", "ShirtNumber", "Status")]}else(temp)
      lineup.catch <- rbind(lineup.catch, temp)
    }

     lineup.catch$PlayerRef <- gsub("p","",lineup.catch$PlayerRef) ## remove P from the player ID
     colnames(lineup.catch)[2] <- "player_id"
    
    FirstName <- xpathSApply(pbpParse, paste("//", "Team/Player/PersonName/First", sep=""), xmlValue)
    LastName <- xpathSApply(pbpParse, paste("//", "Team/Player/PersonName/Last", sep=""), xmlValue)
    FullName <- paste0(FirstName, " ", LastName)
    PID <- xpathSApply(pbpParse, paste("//", "Team/Player", "[@*]", sep=""), xmlAttrs)
    PID <- PID[2,]
    
    ID_DB <- data.frame(player_id = gsub("p","",PID), FirstName, LastName, FullName, stringsAsFactors = F)
    ID_DB <- merge(ID_DB, lineup.catch, by = "player_id")
    
    subs <- xpathSApply(pbpParse, paste("//", "Substitution", "[@*]", sep=""), xmlAttrs)
    subs <- t(subs)
    subs <- as.data.frame(subs[,c("Time", "SubOff", "SubOn")], stringsAsFactors = FALSE)
    subs$SubOff <- gsub("p", "", subs$SubOff)
    subs$SubOn <- gsub("p", "", subs$SubOn)
    
    ID_DB$TimeOn <- ifelse(ID_DB$Status == "Start", 0, NA)
    ID_DB$TimeOff <- ifelse(ID_DB$Status == "Start", TotalMatchTime, NA)
  
    for (p in 1:nrow(ID_DB)) {
      
      player.2.test <- ID_DB[p,]
      if(is.na(player.2.test$TimeOn)){
      time.subbed.on <- if(player.2.test$player_id %in% subs$SubOn){subs %>% filter(player.2.test$player_id == subs$SubOn) %>% select(Time)}else{player.2.test$TimeOn}
      subbed.back.off <- if(player.2.test$player_id %in% subs$SubOff){subs %>% filter(SubOff == player.2.test$player_id) %>% select(Time)}else{NA}
      ID_DB$TimeOff[p] <- ifelse(is.na(time.subbed.on) == FALSE & is.na(subbed.back.off), TotalMatchTime, subbed.back.off)
      ID_DB$TimeOn[p] <- time.subbed.on
      }
    } ## end of for loop
    
     for (p in 1:nrow(ID_DB)) {
      
      player.2.test <- ID_DB[p,]
      if(is.na(player.2.test$TimeOn)){}else{
        
      ID_DB$TimeOff[p] <- if(player.2.test$player_id %in% subs$SubOff){subs %>% filter(SubOff == player.2.test$player_id) %>% select(Time)}else{player.2.test$TimeOff}
        
      }
    } ## end of for loop
   
    ID_DB$TimeOff <- as.numeric(as.character(unlist(ID_DB$TimeOff)))
    ID_DB$TimeOn <- as.numeric(as.character(unlist(ID_DB$TimeOn)))

    ID_DB$Mins.Played <- ID_DB$TimeOff - ID_DB$TimeOn
    ID_DB$MatchID <- MatchID
    
#//------ GameInfo ------------------------------------------------------//
    
    GameData <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "TeamData", "[@*]", sep=""), xmlAttrs)),stringsAsFactors = F)
    colnames(GameData) <- c("Formation", "Score", "team_HA", "team_id")
    GameData$team_id <- gsub("t","", GameData$team_id)
    GameData$team_HA <- ifelse(GameData$team_HA == "Home", 1, 0)

    GameData$team_name <- xpathSApply(pbpParse, paste("//", "Team/Name", sep=""), xmlValue)
    GameData$MatchID <- MatchID
    
    Round <- as.numeric(xpathSApply(pbpParse, paste("//", "Competition/Round", sep=""), xmlValue))
    GameData$Round <- Round
    
    tempx <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "MatchData/Stat", sep=""), xmlValue)) , stringsAsFactors = F)
    colnames(tempx) <- xpathSApply(pbpParse, paste("//", "MatchData/Stat", "[@*]", sep=""), xmlAttrs)
    
    GameData$TotalMatchTime <- tempx$match_time
    
    x <- xpathSApply(pbpParse, paste("//", "MatchInfo/Stat", "[@*]", sep=""), xmlAttrs)
    Matchdf <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "MatchInfo/Stat", sep=""), xmlValue)) , stringsAsFactors = F)
    colnames(Matchdf) <- x
    
    League <- xpathSApply(pbpParse, paste("//", "Competition/Name", sep=""), xmlValue)        
    Country <- xpathSApply(pbpParse, paste("//", "Competition/Country", sep=""), xmlValue)
    Stat.temp <- xpathSApply(pbpParse, paste("//", "Competition/Stat", sep=""), xmlValue)

    GameData$season_id <- Stat.temp[1]
    GameData$season_name <- Stat.temp[2]
    GameData$symid <- Stat.temp[3]
    GameData$matchday <- Stat.temp[4]
    GameData$Competition <- League
    GameData$Country <- Country
    
    GameData$Round <- Round
    
#//------ GameInfo ------------------------------------------------------//

    
    Goals <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "TeamData/Goal", "[@*]", sep=""), xmlAttrs)),stringsAsFactors = F)
    
    Bookings <- as.data.frame(t(xpathSApply(pbpParse, paste("//", "TeamData/Booking", "[@*]", sep=""), xmlAttrs)),stringsAsFactors = F)

    #KitsH <- as.data.frame(t(unlist(xpathSApply(pbpParse, paste("//", "Team/Kit", "[@*]", sep=""), xmlAttrs)[1])), stringsAsFactors = F)
    #KitsH$team_HA <- 1
    #KitsA <- as.data.frame(t(unlist(xpathSApply(pbpParse, paste("//", "Team/Kit", "[@*]", sep=""), xmlAttrs)[2])), stringsAsFactors = F)
    #KitsA$team_HA <- 0
    #Kits <- bind_rows(KitsH, KitsA)
    #colnames(Kits) <- c("id", "colour1", "colour2", "type", "team_HA")
      
    GameCentreTemp <- list(PlayersDB = ID_DB, GameData = GameData, Goals = Goals, Bookings = Bookings)
        return(GameCentreTemp)
    } # end of function 
   