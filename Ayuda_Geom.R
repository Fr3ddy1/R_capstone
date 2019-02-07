####AYUDA GEOM
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tools)
library(ggplot2)
library(leaflet)

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

data <- eq_read_data("signif.txt.tsv")



#limpio la data
eq_clean_data_1<-function(datafram){
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
  eq_location_clean_1(clean_data)
  
}

#arreglo localizacion
eq_location_clean_1<-function(datfram){
  LOCATION_NAME<-NULL
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}


#pruebo
data1 <- eq_clean_data_1(data)
head(data1)


#pruebo
data2 <- eq_location_clean_1(data1)


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
                                 #Required aesthetics
                                 required_aes = c("x"),
                                 #Default aesthetics
                                 default_aes = ggplot2::aes(y = 0.1,
                                                            shape = 21,
                                                            size = 1,
                                                            colour = "blue",
                                                            alpha = 0.8,
                                                            stroke = 1,
                                                            fill = NA),
                                 #Draw key
                                 draw_key = ggplot2::draw_key_point,
                                 #Draw Panel
                                 draw_panel = function(data, panel_scales, coord) {
                                   # Transform the data first
                                   coords <- coord$transform(data, panel_scales)
                                   
                                   #a) Creating the timeline in the x-axis
                                   Timeline_xaxis <- grid::polylineGrob(x = grid::unit(rep(c(0, 1),length(coords$y)),"npc"), 
                                                                             y = rep(coords$y, each = 2),
                                                                             id.length = rep(2,length(coords$y)),
                                                                             gp = grid::gpar(col = "black", lwd = 0.5, lty = 1))
                                   
                                   #b) Creating a point for each Earthquake
                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = alpha(coords$colour, coords$alpha), fill = alpha(coords$fill, coords$alpha)
                                      
                                     
                                   ))
                                   
                                   #Plotting a) y b)
                                   grid::gTree(children = grid::gList(Timeline_xaxis, points))
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
                                           #Required aesthetics
                                           required_aes = c("x", "label"),
                                           #Default aesthetics
                                           default_aes = ggplot2::aes(y = 0.5,
                                                                      number = NULL,
                                                                      max_aes = NULL),
                                           #Draw panel
                                           draw_panel = function(data, panel_scales, coord) {

                                             # Transform the data
                                             coords <- coord$transform(data, panel_scales)

                                             #a) Creating the location where the names will be
                                             Timeline_loc <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"),
                                                                                      y0 = grid::unit(coords$y, "npc"),
                                                                                      x1 = grid::unit(coords$x, "npc"),
                                                                                      y1 = grid::unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                      default.units = "npc",
                                                                                      arrow = NULL,
                                                                                      name = NULL,
                                                                                      gp = grid::gpar(),
                                                                                      vp = NULL)

                                             #2) Adding Text
                                             text <- grid::textGrob(label = coords$label,
                                                                                     x = unit(coords$x, "npc"),
                                                                                     y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                     rot = 60,
                                                                                     just = "left",
                                                                                     gp = grid::gpar(fontsize = 8))

                                             # Plotting a) and b)
                                             grid::gTree(children = grid::gList(Timeline_loc, text))
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
data3_1 <- eq_location_clean_1(eq_clean_data_1(eq_data_read(filename))) %>%
dplyr::filter(datetime >= "1980-01-01" & datetime <="2016-01-01" & COUNTRY == c("USA","CHILE", "VENEZUELA"))

  
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

ggplot(data3_1) +
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
    addMarkers(lng=data$LONGITUDE, lat=data$LATITUDE, popup=as.character(data1[,a]))
  
}

names(data3)
eq_map(data3,"datetime")

#ejemplo añadir label
content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

leaflet() %>% addTiles() %>%
  addMarkers(-122.327298, 47.597131, popup = content
  )

#
contenido <- paste(sep ="<br/>",paste0("<b>","Location: ","</b>",data3$LOCATION_NAME),
                     paste0("<b>","Magnitude: ","</b>",data3$EQ_MAG_ML),
                   paste0("<b>","Total deaths: ","</b>",data3$DEATHS))


#nueva data
data3$pop <- contenido

eq_map(data3,"pop")

#funcion pedida
eq_create_label <- function(data){
  data <- as.data.frame(data)
  contenido <- paste(sep ="<br/>",paste0("<b>","Location: ","</b>",data$LOCATION_NAME),
                     paste0("<b>","Magnitude: ","</b>",data$EQ_MAG_ML),
                     paste0("<b>","Total deaths: ","</b>",data$DEATHS))
  return(contenido)
}

eq_create_label(data3)

##########
##########
##########
##########
#PRUEBO EJEMPLOS CON DATA DE APOYO
source('~/R_capstone/Capstone/R/CopyOffunctions.R')

#
filename<-"signif.txt.tsv"
data3_1 <- eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(datetime >= "1980-01-01" & datetime <="2016-01-01" & COUNTRY == c("USA","CHILE", "VENEZUELA"))

ggplot(data3_1) +
  geom_timeline(aes(x = datetime,  y = COUNTRY,size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))

#
ggplot(data3_1) +
  geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
  geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))


##########
##########
##########
##########
#

#PRUEBO FUNCIONES DEL ARCHIVO FUNCTIONS.R
source('~/R_capstone/Capstone/R/functions.R')

#LEO DATA
#sin comprimir
filename<-"signif.txt.tsv"
#EQ_DATA_READ
data <- eq_read_data(filename)

#LIMPIO DATA
#EQ_CLEAN_DATA
data1 <- eq_clean_data(eq_read_data(filename))


#LIMPIO LOCALIZACIONES
#EQ_CLEAN_LOCATION
data2 <- eq_location_clean(eq_clean_data(eq_read_data(filename)))


#GRAFICO TERREMOTOS SIN NOMBRES
#GEOMTIMELINE
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(DATE >= "1980-01-01" & DATE <="2016-01-01" & COUNTRY == c("USA","CHILE", "VENEZUELA")) %>% 
ggplot() +
  geom_timeline(aes(x = DATE,  y = COUNTRY,size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))


#GRAFICO TERREMOTOS CON NOMBRES
#GEOMTIMELABEL
ggplot(data3_1) +
  geom_timeline(aes(x = DATE, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
  geom_timeline_label(aes(x = DATE, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))

#EQ_MAP
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
dplyr::filter(COUNTRY == "VENEZUELA" & lubridate::year(DATE) >= 1980) %>%
eq_map(name_col = "DATE")

#EQ_CREATE_LABEL
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>%
eq_map(name_col = "popup_text")

###
#DATA COMPRIMIDA
filename <-"earthquakes_data.txt.zip"

#EQ_DATA_READ
data <- eq_read_data(filename)

#LIMPIO DATA
#EQ_CLEAN_DATA
data1 <- eq_clean_data(eq_read_data(filename))


#LIMPIO LOCALIZACIONES
#EQ_CLEAN_LOCATION
data2 <- eq_location_clean(eq_clean_data(eq_read_data(filename)))


#GRAFICO TERREMOTOS SIN NOMBRES
#GEOMTIMELINE
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(DATE >= "1980-01-01" & DATE <="2016-01-01" & COUNTRY == c("USA","CHILE", "VENEZUELA")) %>% 
  ggplot() +
  geom_timeline(aes(x = DATE,  y = COUNTRY,size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))


#GRAFICO TERREMOTOS CON NOMBRES
#GEOMTIMELABEL
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(DATE >= "1980-01-01" & DATE <="2016-01-01" & COUNTRY == c("USA","CHILE", "VENEZUELA")) %>% 
  ggplot() +
  geom_timeline(aes(x = DATE, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
  geom_timeline_label(aes(x = DATE, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))

#EQ_MAP
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(COUNTRY == "VENEZUELA" & lubridate::year(DATE) >= 1980) %>%
  eq_map(name_col = "DATE")

#EQ_CREATE_LABEL
eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(name_col = "popup_text")

