#' Parse an OPTA f24 Files 
#'
#' An OPTA f24 file is an xml file with all of the data for each event of a match.
#' This function takes the file location and parses the XML file into a dataframe. 
#' All qualifiers are in columns named after them, access them using ``. If the
#' qualifier doesn't have a value but was present it is represented by the value 1.
#'
#' @param xml.filename Path to the input file
#' @return A dataframe of the OPTA event data 
#' @export

parse_f24 <- function(xml.filename){
  
  #// Define functions to be used ---------------------------------------------------------//
      
      ## event parsing functions
    
    grab.the.qualifiers <- function(xlm.2.spread) {
    
      Value <- ifelse(is.na(Qualifiers.List["value"]), 1, Qualifiers.List["value"])
      temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
      colnames(temp) <- Qualifiers.List["qualifier_id"]
    
      return(bind_rows(results.temp, temp))
     
    }
    
    ## Pick the Maximum (non-NA) Values
    
    pick.out.the.maximum.values <- function(qualifier.values){
        
        max.values <- list()
        for (c in 1:NCOL(qualifier.values)) {
        col.2.test <- qualifier.values[,c]
        max.val <- col.2.test[!is.na(col.2.test)][1]
        max.values <- append(unlist(max.values), max.val)
        }
        results.Q <- t(as.data.frame(max.values))
        colnames(results.Q) <- colnames(qualifier.values)
        return(results.Q)
    }
    
    ## The Main Unpacking Function
    
    
    convert.event.node.row <- function(xml.2.spread){
      
        ## convert the info in the event node header into a dataframe 
        results <- as.data.frame(t(as.data.frame((xml.2.spread$attrs))))
        rownames(results) <- NULL
    
        ## find the number of qualifiers for this event 
        no.of.qualifiers <- lengths(xml.2.spread$value)
        
        if(no.of.qualifiers > 0){
        ## create a list of qualifiers 
        Qualifier.Unpacked.Step1 <- data.frame(stringsAsFactors = F)
      
        ## loop through each qualifer and pull out the info then bind it to the results .. above 
        for (Q in 1:no.of.qualifiers) {
        Qualifier.unpacked <- unlist(xml.2.spread$value[1][[1]][Q])
        Value <- ifelse(is.na(Qualifier.unpacked["value"]), 1, Qualifier.unpacked["value"])
        temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
        colnames(temp) <- Qualifier.unpacked["qualifier_id"]
        Qualifier.Unpacked.Step1 <- bind_rows(Qualifier.Unpacked.Step1, temp)
        }
        
        ## keep the maximum values in the dataframe (the only none NA values) return as a 
        ## dataframe for use 
         Qualifier.unpacked.df <- pick.out.the.maximum.values(Qualifier.Unpacked.Step1)
         rownames(Qualifier.unpacked.df) <- NULL  
        
        #Qualifier.Unpacked.Step1[1,] <- Qualifier.Unpacked.Step1[is.not.na(Qualifier.Unpacked.Step1)]
        #Qualifier.unpacked.df <- as.data.frame(Qualifier.Unpacked.Step1[1,], stringsAsFactors = F)
        results <- cbind(results, Qualifier.unpacked.df)}
        
        return(results)
    } # end of function 
    
      
  #// Read in the XML File ----------------------------------------------------------------//
  
  pbpParse <- read_xml(xml.filename, encoding = "", as_html = TRUE, options = "NOERROR")
  
  
  #// Spilt the XML File ------------------------------------------------------------------//

    all.event.nodes <- pbpParse %>% 
        xml_find_all('//event') %>% 
        map_df(~list(attrs = list(xml_attrs(.x)), value = list(map(xml_children(.x), xml_attrs))))

  #// Convert all evvents and store in a dataframe ----------------------------------------//

    events <- all.event.nodes %>% 
      split(1:nrow(.)) %>% 
      purrr::map(convert.event.node.row) %>% 
      dplyr::bind_rows()
      
    
  #// Return the resulting dataframe -----------------------------------------------------//

    return(events)
    
} # end of parse.f24 function 
    
