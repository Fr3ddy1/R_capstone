####AYUDA GEOM
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tools)
library(ggplot2)

#signif_txt <- read_delim("Downloads/signif.txt.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
signif_txt <- read_delim(paste(getwd(),"signif.txt.tsv",sep = "/"),"\t", escape_double = FALSE, trim_ws = TRUE)

#leo data
eq_data_read <- function(filename) {
  
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(filename, delim='\t',progress = FALSE)
  })
  dplyr::tbl_df(data)
  
}

#pruebo
data <- eq_data_read("signif.txt.tsv")

#limpio la data
eq_clean_data<-function(datafram){
  COUNTRY <-NULL
  LOCATION_NAME <-NULL
  LATITUDE <-NULL
  LONGITUDE<-NULL
  YEAR<-NULL
  MONTH<-NULL
  DAY<-NULL
  HOUR<-NULL
  EQ_MAG_ML <-NULL
  DEATHS<-NULL
  datetime<-NULL
  #raw_data <- readr::read_delim("/Users/rainier/Desktop/CursoR2/capstone/signif.txt.tsv", 
  #                        col_names=T,delim = "\t",na = "-99")
  
  raw_data <-datafram 
  # "subset to the specific columns that will be required..." 
  clean_data <- raw_data %>%
    # dplyr::filter(FLAG_TSUNAMI != "Tsu") %>%       # taking out the Tsunami's datapoints
    dplyr::select(COUNTRY,LOCATION_NAME, LATITUDE, LONGITUDE,YEAR, MONTH, DAY, HOUR, EQ_MAG_ML,DEATHS) %>%
    dplyr::mutate_each(funs(gsub(".*:", "", LOCATION_NAME)),LOCATION_NAME)%>%
    dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE))%>%
    tidyr::unite(datetime, YEAR, MONTH, DAY, HOUR) %>%
    dplyr::mutate(datetime = lubridate::ymd_h(datetime))%>%
    dplyr::mutate(DEATHS=as.numeric(DEATHS))
  rm(raw_data)
  #returning the cleaned data
  eq_location_clean(clean_data)
  
}

#arreglo localizacion
eq_location_clean<-function(datfram){
  LOCATION_NAME<-NULL
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}


#pruebo
data1 <- eq_clean_data(data)
head(data1)


#pruebo
data2 <- eq_location_clean(data1)


#defino geom
geom_timeline <- function(mapping = NULL, 
                          data = NULL, 
                          na.rm = TRUE,
                          position = "identity",
                          stat = "identity",
                          show.legend = NA, 
                          inherit.aes = TRUE, ...) {
    ggplot2::layer(
    geom = GeomTimeline, 
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}

#
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 #<character vector of required aesthetics>
                                 required_aes = c("x"),
                                 #aes(<default values for certain aesthetics>)
                                 default_aes = ggplot2::aes(y = 0.1,
                                                            shape = 21,
                                                            size = 1,
                                                            colour = "blue",
                                                            alpha = 0.8,
                                                            stroke = 1,
                                                            fill = NA),
                                 #<a function used to draw the key in the legend>
                                 draw_key = ggplot2::draw_key_point,
                                 ## Function that returns a grid grob that will 
                                 ## be plotted (this is where the real work occurs)
                                 draw_panel = function(data, panel_scales, coord) {
                                   # Transform the data first
                                   coords <- coord$transform(data, panel_scales)
                                   
                                   #To create the Earthquake's timeline we will separate the task in two parts
                                   #1) The line over the X-axis from where it will be plotted the Earthquakes as Points
                                   #2) The points for each Earthquake of a given Country in between two dates (years)
                                   #The use of the Concept of Grobs
                                   
                                   # 1) Creating the X-axis line (timeline)
                                   Timeline_line_grobs <- grid::polylineGrob(x = grid::unit(rep(c(0, 1),
                                                                                                length(coords$y)),
                                                                                            "npc"), 
                                                                             y = rep(coords$y, each = 2),
                                                                             id.length = rep(2,length(coords$y)),
                                                                             gp = grid::gpar(col = "black", lwd = 0.3, lty = 1))
                                   
                                   # 2) Creating the points for each Earthquake of a Given Country
                                   Earthquakes_points_grobs <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = alpha(coords$colour, coords$alpha), fill = alpha(coords$fill, coords$alpha)
                                                     #,
                                                     #lwd = coords$stroke * .stroke / 2),
                                     #fontsize = coords$size * .pt + coords$stroke * .stroke / 2
                                     
                                   ))
                                   
                                   # Plotting both the Timeline (X-axis) and the Eartquakes Points
                                   grid::gTree(children = grid::gList(Timeline_line_grobs, Earthquakes_points_grobs))
                                 })

#defino el otro geom
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                na.rm = TRUE,
                                show.legend = NA,
                                stat = "identity",
                                position = "identity",
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeLineAnnotation,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# #
GeomTimeLineAnnotation <- ggplot2::ggproto("GeomTimeLineAnnotation", ggplot2::Geom,
                                           #<character vector of required aesthetics>
                                           required_aes = c("x", "label"),
                                           #aes(<default values for certain aesthetics>)
                                           default_aes = ggplot2::aes(y = 0.5,
                                                                      number = NULL,
                                                                      max_aes = NULL),
                                           #<a function used to draw the key in the legend>
                                            #draw_key = draw_key_label,
                                           ## Function that returns a grid grob that will
                                           ## be plotted (this is where the real work occurs)
                                           draw_panel = function(data, panel_scales, coord) {

                                             # Transform the data
                                             coords <- coord$transform(data, panel_scales)

                                             #To create the Earthquake's timeline with annothation we will separate the task in two parts
                                             #1) we will locate where the tags should be places and then
                                             #2) To add the annotation labels to the layer

                                             #1) Creating the location in the timelines (X-axis) where the location names will be placed
                                             Timeline_seg_grobs <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"),
                                                                                      y0 = grid::unit(coords$y, "npc"),
                                                                                      x1 = grid::unit(coords$x, "npc"),
                                                                                      y1 = grid::unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                      default.units = "npc",
                                                                                      arrow = NULL,
                                                                                      name = NULL,
                                                                                      gp = grid::gpar(),
                                                                                      vp = NULL)

                                             #2) Adding the text to the grid
                                             Earthquake_text_grobs <- grid::textGrob(label = coords$label,
                                                                                     x = unit(coords$x, "npc"),
                                                                                     y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                     rot = 60,
                                                                                     just = "left",
                                                                                     gp = grid::gpar(fontsize = 8))

                                             # Plotting the Eartquakes location label over the timeline
                                             grid::gTree(children = grid::gList(Timeline_seg_grobs, Earthquake_text_grobs))
                                           }
)



# filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
# eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
# dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("MEXICO","USA", "JORDAN"))%>%
# ggplot() +
# geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))
# }


#data sin comprimir
#por alguna razon no funciona
filename<-"signif.txt.tsv"
data3_1 <- eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("ECUADOR","CHILE", "VENEZUELA"))

  
ggplot(data3_1) +
geom_timeline(aes(x = datetime,  y = COUNTRY,size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))


#DATA COMPRIMIDA
#ESTA ES LA QUE FUNCIONA
#GRAFICO SIN LEYENDA
filename<-paste(getwd(),"earthquakes_data.txt.zip",sep = "/")
data3 <- eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
  dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("ECUADOR","CHILE", "VENEZUELA"))


ggplot(data3) +
  geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) 


#GRAFICO CON LEYENDA
ggplot(data3) +
  geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
  geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))


###SEMANA 3 
#LEAFLET MAP
library(leaflet)

#ejemplo
m <- leaflet() %>%
       addTiles() %>%  # Add default OpenStreetMap map tiles
       addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

m

#mi caso
#uso data3
m1 <- leaflet(data = data3) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=data3$LONGITUDE, lat=data3$LATITUDE, popup=paste(data3$LOCATION_NAME,data3$COUNTRY))


m1

#creo funcion
eq_map <- function(data,annot_col){
  a <- which(annot_col==names(data))
  data1 <- as.data.frame(data)
  leaflet(data = data) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=data$LONGITUDE, lat=data$LATITUDE, popup=data1[,a])
  
}

names(data3)
eq_map(data3,"COUNTRY")

