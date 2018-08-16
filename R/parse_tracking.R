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
  
            dat <- read.table(tracking.xml, stringsAsFactors = F)
            dat <- dat$V1
            
            metadata <- parse_CH_metadata(metaXML)
            
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
        
        
        newdat = dat %>%
                    unlist()%>%
                    read.table(text = .,sep = ':')%>%
                    as.matrix()%>%
                    gsub(';','\n',.)%>%
                    data.frame(stringsAsFactors = FALSE)
        
        dat1 = newdat[-3]%>%
                     group_by(V1)%>%
                     mutate(V2=list(read.csv(text=V2,header = F,stringsAsFactors = F)))%>%
                     unnest()
        
        dat2 = newdat[-2]%>%
                 group_by(V1)%>%
                 mutate(V3=list(read.csv(text=V3,header = F,stringsAsFactors = F)))%>%
                 unnest()
        
        cat("// Dat file split....")
        
        dat1 <- as.data.frame(dat1, stringsAsFactors = F )
        dat2 <- as.data.frame(dat2, stringsAsFactors = F )
        
        colnames(dat1) <- c("FrameID", "removeCol", "team_HA", "TrackID", "JerseyNo", "x", "y", "speed")
        colnames(dat2) <- c("FrameID", "removeCol", "x", "y", "z", "speed", "Ball.Ownership", "Ball.InPlay", "Ball.Contact.Info1")
        
        dat1$z <- 20
        dat2$team_HA <- 10
        dat2$TrackID <- 50
        dat2$JerseyNo <- NA
        
        dat1 <- merge(dat1, dat2[c("FrameID", "Ball.Ownership", "Ball.InPlay", "Ball.Contact.Info1")], by= "FrameID")
        
        dat1$TrackID <- as.numeric(as.character(dat1$TrackID))
        dat2$TrackID <- as.numeric(as.character(dat2$TrackID))
        
        dat.full <- bind_rows(dat1, dat2)
        
        dat.full <- within(dat.full, rm(removeCol))
        
        cat("// Tracking data merged....||")

        return(dat.full)
        
} 


