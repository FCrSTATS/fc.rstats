# fc.rstats
A package with various functions that help people working with OPTA and ChyronHego data, this will be a regularly updated with new functions. 

### parse_f24()
##### Parse an OPTA f24 Files 
An OPTA f24 file is an xml file with all of the data for each event of a match. The parse_24() function takes the file location and parses the XML file converting it into a dataframe. All qualifiers are in columns named after them. If the qualifier doesn't have a value but was present it is represented by the value 1.

### parse_CH_metadata()
##### Parse an ChyronHego Metadata File 
A ChyronHego Metadata file contains key information in order to use the actual Tracking data. This simple function takes the path to an xml file and returns a parsed dataframe.

### create_CH_pitch()
##### Create a Pitch ready for Chyronhego data
Chyronhego tracking data working with pitch length of 10500 and width of 6600. Uses ggplot and returns a plot ready for further data to be plotted over the top.   

![](https://github.com/FCrSTATS/fc.rstats/blob/master/images/CHpitchoptions.jpg)

