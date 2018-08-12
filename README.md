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

## Data Preparation Functions 

### add_TimeSync()
**Add a TimeSync variable :** The OPTA and ChyronHego datasets don't share a common clock. OPTA data is recorded in seconds and ChyronHego
data is recorded at 25 frames a second. The addition of a TimeSync variable to the events dataframe allows the two dataframes to be synced and linked together easier with a common clock.

### add_attacking_direction() 
**Add the attacking direction to events:** Adding a variable of 'attacking.direction' to the events dataframe allows us to keep track of the direction of play and how that relates to x, y positioning. It's a basic but crucial variable to add to our events dataframe to improve our analysis.

### add_CH_origin()
**Convert the pass origin from OPTA to ChyronHego spec:** OPTA event data has two variables x and y, that represent the the x and y pitch coordinates for the origina point of an event. We can convert from OPTA 0-100 values to ChyronHego CM and return the events dataframe back with new variables of origin.x and origin.y. The attacking.direction needs to be added to the events dataframe first from the 'add_attacking_direction' function in this package. 

### add_pass_target()
**Convert the pass destination from OPTA to ChyronHego spec:** OPTA event data has two variables x and y, that represent the the x and y pitch coordinates for the origina point of an event. We can convert from OPTA 0-100 values to ChyronHego CM and return the events dataframe back with new variables of origin.x and origin.y. The attacking.direction needs to be added to the events dataframe first from the 'add_attacking_direction' function in this package. 

## Plotting Functions 

### create_CH_pitch()
**Create a Pitch ready for Chyronhego data:** Chyronhego tracking data working with pitch length of 10500 and width of 6600. Uses ggplot and returns a plot ready for further data to be plotted over the top.   

### create_OPTA_pitch()
**Create a Pitch ready for OPTA data:** A pitch plot base that is designed for the OPTA values for x and y of 0-100. Uses ggplot and returns a plot ready for further data to be plotted over the top.   

Both create_CH_pitch() and create_OPTA_pitch() functions are customisable with various options: 
![](https://github.com/FCrSTATS/fc.rstats/blob/master/images/CHpitchoptions.jpg)

 
