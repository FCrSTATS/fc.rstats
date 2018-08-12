#' Create a Pitch ready for OPTA data
#'
#' Chyronhego tracking data working with pitch OPTA values for x and y of 9-100
#' Uses ggplot and returns a plot ready for further data to be plotted over the top.   
#' @param grass_colour Colour of the grass, Default = "#F9F9F9", HEXCODE or accept colour string i.e. "red" 
#' @param line_colour Colour of the line, Default = "#8F8F8F", HEXCODE or accept colour string i.e. "red" 
#' @param background_colour Colour of the Default = "#F9F9F9", border area, HEXCODE or accept colour string i.e. "red" 
#' @param goaltype Type of goal used. Default = "barcanumbers". "line" and "box" also available
#' @param middlethird Default = FALSE, Adds a shading to the middle third of the pitch 
#' @param BasicFeatures Default = FALSE, If TRUE removes most of the pitch features to have a more minimalist design
#' @param arcs Default = TRUE, adds the D-arcs
#' @param padding Default = 10, adds some padding around the pitch
#' @return A pitch plot, that allows data to be plotted on top 
#' @export

create_OPTA_pitch <- function(grass_colour = "#F9F9F9", 
                         line_colour = "#8F8F8F", 
                         background_colour = "#F9F9F9", 
                         goal_colour = "#000000", 
                         goaltype = "barcanumbers", 
                         middlethird = TRUE, 
                         BasicFeatures = FALSE, 
                         arcs = TRUE, 
                         padding = 5){

  ## blank pitch theme 
    ## set theme for blank pitch
    theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ## Basic Plotting 
  p <- ggplot() + xlim(c(0-padding,100+padding)) + ylim(c(0-padding,100+padding)) + theme_blankPitch()
  p
  
  ## Add Middle Third Rectangle 
  if(middlethird == TRUE){p <- p + geom_rect(aes(xmin=33.3, xmax=66.6, ymin=0, ymax=100), colour = NA, fill = "black", alpha = 0.10)}else{}
  
  ## Basic Features
  p <- p +
  # add the base rectangle of the pitch
  geom_rect(aes(xmin=0, xmax=100, ymin=0, ymax=100), fill = NA, colour = line_colour) +
  # add the 18 yard box defensive
  geom_rect(aes(xmin=0, xmax=17, ymin=21.1, ymax=78.9), fill = grass_colour, colour = line_colour) +
  # add the 18 yard box offensive
  geom_rect(aes(xmin=83, xmax=100, ymin=21.1, ymax=78.9), fill = grass_colour, colour = line_colour) +
  # add halway line
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100),colour = line_colour)
    
  ## Add Goals 
  
  ## LINE TYPE
  if(goaltype == "line"){
  p <- p +
  # add the goal Defensive
  geom_segment(aes(x = 0, y = 45.2, xend = 0, yend = 54.8),colour = goal_colour, size = 1) +
  # add the goal offensive
  geom_segment(aes(x = 100, y = 45.2, xend = 100, yend = 54.8),colour = goal_colour, size = 1)

  }else{}

  ## Barca Numbers TYPE
  if(goaltype == "barcanumbers"){
  p <- p +
  # add the goal Defensive
  geom_segment(aes(x = -1, y = 45.2, xend = -1, yend = 54.8),colour = line_colour, size = 1) +
  # add the goal offensive
  geom_segment(aes(x = 101, y = 45.2, xend = 101, yend = 54.8),colour = line_colour, size = 1)

  }else{}

  ## BOX TYPE
  if(goaltype == "box"){
  p <- p +
  # add the goal Defensive
  geom_rect(aes(xmin = -1 , ymin = 45.2, xmax = 0 , ymax =  54.8), fill = grass_colour, colour = line_colour) +
  # add the goal offensive
  geom_rect(aes(xmin = 100, ymin = 45.2, xmax = 101, ymax =  54.8), fill = grass_colour, colour = line_colour)
  }else{}

  ## Add extra features 
  if(BasicFeatures == FALSE){
  p <- p + 
  # add the six yard box Defensive
  geom_rect(aes(xmin=0, xmax=5.8, ymin=36.8, ymax=63.2), fill = grass_colour, colour = line_colour)  +
  # add the six yard box offensive
  geom_rect(aes(xmin=94.2, xmax=100, ymin=36.8, ymax=63.2), fill = grass_colour, colour = line_colour) +
  # add centre circle
  annotate("path",
     x=50+8.7*cos(seq(0,2*pi,length.out=100)),
     y=50+8.7*sin(seq(0,2*pi,length.out=100)),col = line_colour) + 
  # add penalty spot left
  geom_point(aes(x = 11.5 , y = 50), colour = line_colour, size = 0.75) +
  # add penalty spot right
  geom_point(aes(x = 88.5 , y = 50), colour = line_colour, size = 0.75) +
  # add centre spot
  geom_point(aes(x = 50 , y = 50), colour = line_colour, size = 0.75)
  }else{}

  ## Add the arcs 
  if(arcs == TRUE){
  p <- p +
  # vertical tram lines
  annotate("path",
           x = (11.5) + 9.2 * cos(seq(-0.3*pi, 0.3*pi, length.out = 500)),
           y = 50 + 9.2 * sin(seq(-0.3*pi, 0.3*pi, length.out = 500)),
           col = line_colour) +
  annotate("path",
           x = (88.5) - 9.2 * cos(seq(-0.3*pi, 0.3*pi, length.out = 500)),
           y = 50 - 9.2  * sin(seq(-0.3*pi, 0.3*pi, length.out = 500)),
           col = line_colour)
  }else{}
  
  ## Add attacking direction 
  p <- p + 
  geom_segment(aes(x = 40, y = -2, xend = 60, yend = -2),colour = line_colour, arrow = arrow(length = unit(0.1, "cm"), type="closed"))
  ## JdeP --- add at later date 

  ## return plot 
  return(p)

}
