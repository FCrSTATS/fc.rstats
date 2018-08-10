#' Parse an ChyronHego Metadata File 
#'
#' A ChyronHego Metadata file contains key information in order to use the actual 
#' Tracking data. This simple function takes the path to an xml file and returns a
#' a parsed dataframe.
#'
#' @param xml.filename Path to the input file
#' @return A dataframe of the Tracking metadata 
#' @export

parse_CH_metadata <- function(xml.filename){
  
      grabAll <- function(XML.parsed, field){
      parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep=""))
      results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
      if(typeof(results)=="list"){
        do.call(rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
      } else {
        as.data.frame(results, stringsAsFactors=F)
      }
    }
    
    library(XML)
    pbpParse <- xmlInternalTreeParse(xml.filename)
    temp <- grabAll(pbpParse, "match")
    period.info <- grabAll(pbpParse, "period")
    temp$period1Start <- as.numeric(as.character(period.info$iStartFrame[1]))
    temp$period1End <- as.numeric(as.character(period.info$iEndFrame[1]))
    temp$period2Start <- as.numeric(as.character(period.info$iStartFrame[2]))
    temp$period2End <- as.numeric(as.character(period.info$iEndFrame[2]))
    temp$period3Start <- as.numeric(as.character(period.info$iStartFrame[3]))
    temp$period3End <- as.numeric(as.character(period.info$iEndFrame[3]))
    temp$period4Start <- as.numeric(as.character(period.info$iStartFrame[4]))
    temp$period4End <- as.numeric(as.character(period.info$iEndFrame[4]))
    
    return(temp)
}
