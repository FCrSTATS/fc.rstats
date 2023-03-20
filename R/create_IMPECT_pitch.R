#' Create a Pitch ready for IMPECT data
#'
#' IMPECT event data working with pitch length of 105 and width of 68. 
#' Uses ggplot and returns a plot ready for further data to be plotted over the top.   
#' @param grass_colour Colour of the grass, Default = "#F9F9F9", HEXCODE or accept colour string i.e. "red" 
#' @param line_colour Colour of the line, Default = "#8F8F8F", HEXCODE or accept colour string i.e. "red" 
#' @param background_colour Colour of the Default = "#F9F9F9", border area, HEXCODE or accept colour string i.e. "red" 
#' @param goaltype Type of goal used. Default = "barcanumbers". "line" and "box" also available
#' @param middlethird Default = FALSE, Adds a shading to the middle third of the pitch 
#' @param BasicFeatures Default = FALSE, If TRUE removes most of the pitch features to have a more minimalist design
#' @param arcs Default = TRUE, adds the D-arcs
#' @param padding Default = 100, adds some padding around the pitch
#' @return A pitch plot, that allows data to be plotted on top 
#' @export


create_IMPECT_pitch <- function(grass_colour = "#F9F9F9", 
                         line_colour = "#8F8F8F", 
                         background_colour = "#F9F9F9", 
                         goal_colour = "#000000", 
                         goaltype = "barcanumbers", 
                         middlethird = FALSE, 
                         BasicFeatures = FALSE, 
                         arcs = TRUE, 
                         padding = 100){

  ## basic pitch dimensions  
  min.x <- -52.5
  max.x <- 52.5
  min.y <- -34
  max.y <- 34
  length.x <- 105
  width.y <- 68  
  
  ## Defining features along the length
  boxEdge <- 16.5
  sixYard <- 5.5
  penSpot <- 11

  ## Defining features along the width
  boxWidth <- 40.32
  goalWidth <- 7.32
  post2.6yard <- 5.5
  post2.18yard <- 16.5
  
  ## calculated dimensions 
  centre.2.18 <- (goalWidth/2) + post2.18yard
  centre.2.6 <- (goalWidth/2) + post2.6yard

  ## other dimensions
  centreCirle_d <- 9.15
  corner.flag.d <- 1
  
  ## blank pitch theme 
    ## set theme for blank pitch
    theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ## Basic Plotting 
  p <- ggplot() + xlim(c(min.x-padding,max.x+padding)) + ylim(c(min.y-padding,max.y+padding)) + theme_blankPitch()
  p
  
  ## Add Middle Third Rectangle 
  if(middlethird == TRUE){p <- p + geom_rect(aes(xmin=(-(length.x/3)/2), xmax=((length.x/3)/2), ymin=min.y, ymax=max.y), colour = NA, fill = "black", alpha = 0.10)}else{}
  
  ## Basic Features
  p <- p +
  # add the base rectangle of the pitch
  geom_rect(aes(xmin=min.x, xmax=max.x, ymin=min.y, ymax=max.y), fill = NA, colour = line_colour) +
  # add the 18 yard box defensive
  geom_rect(aes(xmin=min.x, xmax=min.x+boxEdge, ymin=-centre.2.18, ymax=centre.2.18), fill = grass_colour, colour = line_colour) +
  # add the 18 yard box offensive
  geom_rect(aes(xmin=max.x, xmax=max.x-boxEdge, ymin=-centre.2.18, ymax=centre.2.18), fill = grass_colour, colour = line_colour) +
  # add halway line
  geom_segment(aes(x = 0, y = min.y, xend = 0, yend = max.y),colour = line_colour)
    
  ## Add Goals 
  
  ## LINE TYPE
  if(goaltype == "line"){
  p <- p +
  # add the goal Defensive
  geom_segment(aes(x = min.x, y = -goalWidth/2, xend = min.x, yend = goalWidth/2),colour = goal_colour, size = 1) +
  # add the goal offensive
  geom_segment(aes(x = max.x, y = -goalWidth/2, xend = max.x, yend = goalWidth/2),colour = goal_colour, size = 1)

  }else{}

  ## Barca Numbers TYPE
  if(goaltype == "barcanumbers"){
  p <- p +
  # add the goal Defensive
  geom_segment(aes(x = min.x-80, y = -goalWidth/2, xend = min.x-80, yend = goalWidth/2),colour = line_colour, size = 1) +
  # add the goal offensive
  geom_segment(aes(x = max.x+80, y = -goalWidth/2, xend = max.x+80, yend = goalWidth/2),colour = line_colour, size = 1)

  }else{}

  ## BOX TYPE
  if(goaltype == "box"){
  p <- p +
  # add the goal Defensive
  geom_rect(aes(xmin = min.x-80 , ymin =  -goalWidth/2, xmax = min.x , ymax =  goalWidth/2), fill = grass_colour, colour = line_colour) +
  # add the goal offensive
  geom_rect(aes(xmin = max.x, ymin =  -goalWidth/2, xmax = max.x+80, ymax =  goalWidth/2), fill = grass_colour, colour = line_colour)
  }else{}

  ## Add extra features 
  if(BasicFeatures == FALSE){
  p <- p + 
  # add the six yard box Defensive
  geom_rect(aes(xmin=min.x, xmax=min.x+sixYard, ymin=-centre.2.6, ymax=centre.2.6), fill = grass_colour, colour = line_colour)  +
  # add the six yard box offensive
  geom_rect(aes(xmin=max.x-sixYard, xmax=max.x, ymin=-centre.2.6, ymax=centre.2.6), fill = grass_colour, colour = line_colour) +
  # add centre circle
  annotate("path",
           x = 915 * cos(seq(-pi, pi, length.out = 300)),
           y = 1100 * sin(seq(-pi, pi, length.out = 300)),
           col = line_colour) + 
  # add penalty spot left
  geom_point(aes(x = min.x + 1100 , y = 0), colour = line_colour, size = 0.75) +
  # add penalty spot right
  geom_point(aes(x = max.x - 1100 , y = 0), colour = line_colour, size = 0.75) +
  # add centre spot
  geom_point(aes(x = 0 , y = 0), colour = line_colour, size = 0.75)
  }else{}

  ## Add the arcs 
  if(arcs == TRUE){
  p <- p +
  # vertical tram lines
  annotate("path",
           x = (min.x + 1100) + 915 * cos(seq(-0.3*pi, 0.3*pi, length.out = 300)),
           y = 1100 * sin(seq(-0.3*pi, 0.3*pi, length.out = 300)),
           col = line_colour) +
  annotate("path",
           x = (max.x - 1100) - 915 * cos(seq(-0.3*pi, 0.3*pi, length.out = 300)),
           y = 1100 * sin(seq(-0.3*pi, 0.3*pi, length.out = 300)),
           col = line_colour)
  }else{}
  
  ## Add attacking direction 
  p <- p + 
  geom_segment(aes(x = -1000, y = min.y-100, xend = 1000, yend = min.y-100),colour = line_colour, arrow = arrow(length = unit(0.1, "cm"), type="closed"))

  ## return plot 
  return(p)
}