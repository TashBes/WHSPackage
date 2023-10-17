### all functions required to map the whs ###

#' Map of any shape file overlayed with the world heritage site polygon and a map of South Africa
#'
#' Maps the shape file chosen, e.g. MPA's, and adds an annotaed map of South Africa and a line around the proposed world heritage site. The polygons within the shape file are coloured based on the column of choice, and named accordingly. The north arrow and scale bar are added to the bottown right, and a legend of the shape files polygons is added. After it has been mapped a png of the map is saved to outputs.
#' @param data The shape file to be mapped
#' @param col The column in the shape file that represents the polygons
#' @param name The name of the polygons
#' @details The packages tidyverse and rnaturalearth need to be installed
#' @export
map <- function(data, col, name) {  #this tells you the things in the function that you must set each time

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # get a shape file for the world

  map <- ggplot() +                              #open a map

    geom_sf(data = whs_extent,                   #chose data for the first layer of the map - eg. whs polygon outline
            lwd = 0.7,                           #choose thickness of the line
            color= "red",                        #choose the line colour
            fill = NA) +                         #choose the background colour

    theme_bw() +                                 #choose a theme for the whole map (this one is classic)

    geom_sf(data = world,                        #choose data for the next layer - world.shp
            fill= NA) +                          #choose the background colour


    geom_sf(data = data,            #choose data for the next layer (this will be whatever you put into your function)
            aes(fill= col),         #this will colour the polygons based on the column you choose
            show.legend = T)+       #do you want to show the legend (T or F)

    annotate(geom = "text",                      #this creates text in your map
             x = 22,                             #choose the longitude of the text position
             y = -32,                            #choose the latitude of the text position
             label = "South Africa",             #choose the text to be displayed
             #choose either italic or bold or nothing for normal
             color = "black",                    #choose the colour of the letters
             size = 8) +                         #choose the size of the letters

    annotation_scale(location = "br",            #location of the scale bar (br = bottom right)
                     width_hint = 0.1,           #proportion of plot that scalebar occupies
                     style= "bar") +

    annotation_north_arrow(location = "br",                      #location of arrow (br = bottom right)
                           which_north = "true",                 #points to the north pole
                           height = unit(1, "cm"),
                           width = unit(1, "cm"),
                           pad_x = unit(0.1, "in"),              #margin between arrow and map edge
                           pad_y = unit(0.3, "in"),              #margin between arrow and map edge
                           style = north_arrow_orienteering) +   #style of arrow

    coord_sf(xlim = c(15.00, 33.00),             #choose the longitude limits for the map
             ylim = c(-26.00, -37.00),           #choose the latitude limits for the map
             expand = T) +

    ggtitle(paste("Map of the",#put a title on top of the map (name is what you put into the function)
                  paste0(name,"'s"),
                  "within the proposed World Heritage Site")) +

    theme(legend.title = element_blank(),                     #make the legend title blank (go away)
          legend.text = element_text(size = 6),                   #legend text size
          legend.key.size = unit(0.8,"line"),                     #legend shape size
          legend.position = "bottom",                             #legend position
          panel.grid.major = element_blank(),                     #grid lines, colour and opacity
          axis.title = element_blank())                           #remove axis titles


  ggsave(here(paste0("outputs/",name,".png")))                    #save as a png
  map                                                             # print the map
}

#   map2 <- ggplot()+
#    geom_sf(data = world, fill = "grey30")+
#    geom_rect(aes(xmin=16.00, xmax=33.00, ymin= 26, ymax= 37),
#              alpha=0,
#              colour="red",
#              size=1,
#              linetype=1)+
#  xlim(-15, 50) +
#  ylim(40, -35)+
#  labs(x = NULL, y = NULL) +
#  theme_bw() +
#  theme(axis.text = element_blank(),
#        axis.title=element_blank(),
#        axis.ticks = element_blank(),
#        axis.ticks.length = unit(0, "pt"),
#        plot.margin = margin(0, 0, 0, 0, "cm"),
#        panel.grid.major = element_blank())


#map_insert <- ggdraw() +
# draw_plot(map) +
# draw_plot(map2, x = 0.75, y = 0.66, width = 0.2, height = 0.25)
#map_insert
