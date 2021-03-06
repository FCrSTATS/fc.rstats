% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_f7_multiple.R
\name{parse_f7_multiple}
\alias{parse_f7_multiple}
\title{Parse Multiple OPTA f7 Files}
\usage{
parse_f7_multiple(f7.xml)
}
\arguments{
\item{f7.xml}{Path to the input file}
}
\value{
A list of dataframes containing match information
}
\description{
An OPTA f7 file is an xml file with various information about a match such as 
lineups, goals, cards, kits used and more. This function takes the file locations 
and parses the XML file into a dataframes which are stored in a list. This function
only works if you have a file location similar to "Database/f7/999999_f7.xml"
}
