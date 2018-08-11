# fc.rstats
A package with various functions that help people working with OPTA and ChyronHego data, this will be a regularly updated with new functions. Installed via: 

``` r
devtools::install_github("FCrSTATS/fc.rstats")
```

## Parsing Functions 

### parse_f24()
**Parse an OPTA f24 Files:** An OPTA f24 file is an xml file with all of the data for each event of a match. The parse_24() function takes the file location and parses the XML file converting it into a dataframe. All qualifiers are in columns named after them. If the qualifier doesn't have a value but was present it is represented by the value 1.

### parse_f7()
**Parse an OPTA f7 Files:** OPTA f7 file is an xml file with various information about a match such as lineups, goals, cards, kits used and more. This function takes the file location and parses the XML file into a dataframes which are stored in a list. 


### parse_CH_metadata()
**Parse an ChyronHego Metadata File:** A ChyronHego Metadata file contains key information in order to use the actual Tracking data. This simple function takes the path to an xml file and returns a parsed dataframe.


### parse_tracking()
**Parse an ChyronHego Tracking Data File:** ChyronHego's tracking data contains positional data at 25 frames per second. The raw data comes in .dat format with all players, officials and ball positional data per frame combined in a string. This function spilts up the string and arranges the resulting data in a logical format. 


## Plotting Functions 

### create_CH_pitch()
**Create a Pitch ready for Chyronhego data:** Chyronhego tracking data working with pitch length of 10500 and width of 6600. Uses ggplot and returns a plot ready for further data to be plotted over the top.   

![](https://github.com/FCrSTATS/fc.rstats/blob/master/images/CHpitchoptions.jpg)

 
