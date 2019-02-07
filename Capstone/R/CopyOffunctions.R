#' Mastering Software Development in R Specialization Capstone Project
#' Coursera Capstone Project

#' Read Earthquakes data.
#'
#' @param filename A caracter that contains the name of the file with its respective
#' extension.
#'
#' @return This function returns the read data in a "dataframe", "tbl_df", "tbl" format.
#'
#' @note This function will generate an error if the filemane is wrong or have a
#' wrong extension.
#'
#' @importFrom readr read_delim
#' @import dplyr 
#'
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_read_data(file)
#' }
#' @export
eq_read_data <- function(filename) {
  
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(filename, delim='\t',progress = FALSE)
  })
  dplyr::tbl_df(data)
  
}

#' Function to clean Earthquake dataframe
#' 
#' @param data  dataframe obtained from the eq_read_data
#' @return  dataframe with a new DATE, LATITUDE and LONGITUDE column
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_clean_data(eq_read_data(file))
#' }
#'
#' @export
eq_clean_data <- function(datafram){
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

#' Function for title case the Earthquake's Location Data-Name
#' @param data dataframe with location names written in Uper case
#' @return  dataframe with Tittle Case Location names
#' @importFrom stringr str_remove
#' @importFrom tools toTitleCase
#'@examples
#'\dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_location_clean(eq_clean_data(eq_read_data(file)))
#' }
#'
#' @export
eq_location_clean <- function(datfram){
  LOCATION_NAME<-NULL
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}

# Function that will use the GeomTimeLine Prototype Function required to Plot a Timeline with the Earthquakes of a given country
#' @param mapping aesthetic mappings 
#' @param data dataframe that contains the Earthquake's data
#' @param na.rm  removes the NA values from the data frame
#' @param position position adjustment 
#' @param stat The Layer's statistical transformation 
#' @param show.legend layer's legend
#' @param inherit.aes indicate the default aesthetics 
#' @param ... other arguments
#' @return Plot an Earthquakes timeline which contains the Earthquakes of a country o countries  between two dates
#' @import ggplot2
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_location_clean(eq_clean_data(eq_read_data(file))) %>%
#' dplyr::filter(datetime >= "1986-02-01" & datetime <="2016-06-01" & COUNTRY == c("ECUADOR","CHILE", "VENEZUELA"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))
#' }
#'
#' @export
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

#' Function to plot an Earthquake's Location timeline 
#' The GeomTimeLine will use a dataframe obtained from the function eq_clean_data. 
#' This Geom will return a plot with the earthquakes of a country or countries between two dates
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


#' Function that add the Eartquakes's Location labels to an timeline earthquake
#' @param mapping aesthetic mappings 
#' @param data dataframe that contains the Earthquake's data
#' @param na.rm  removes the NA values from the data frame
#' @param position position adjustment 
#' @param stat The Layer's statistical transformation 
#' @param show.legend layer's legend
#' @param inherit.aes indicate the default aesthetics 
#' @param ... other arguments
#' @return Plot an Earthquakes timeline which contains the Earthquakes of a country o countries  between two dates
#' @import ggplot2
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_location_clean(eq_clean_data(eq_read_data(file))) %>%
#' dplyr::filter(datetime >= "1986-02-01" & datetime <="2016-06-01" & COUNTRY == c("ECUADOR","CHILE", "VENEZUELA"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))+
#' geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))
#'}
#' @export
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

#' Function to add labels on a Earthquake's Location timeline 
#' This Geom will return a plot with the earthquakes of a country or countries between two dates with its respectives names
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


#' Function that plot the earthquake Data in an Interactive Map.
#'
#' The Earthquakes will be plot using its longitude and latitude information. The user
#' can select a column in order to obtain information about the earthquake.
#'
#' @param data Clean earthquake data.
#' @param name_col Name of a column which will be use to show information about the earthquake.
#'
#' @return This function returns an interactive map.
#'
#' @note If an invalid column name is provided, the function provides a warning
#' and uses the LOCATION_NAME column as teh annotation column.
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_location_clean(eq_clean_data(eq_read_data(file))) %>%
#' dplyr::filter(COUNTRY == "VENEZUELA" & lubridate::year(datetime) >= 1980) %>%
#' eq_map(annot_col = "datetime")
#' }
#'
#' @export
eq_map <- function(data,name_col){
  #check
  if(!(any(data %in% colnames(eq_clean)))) {
    warning("Invalid Column")
    annot_col = "datetime"
  }
  a <- which(name_col==names(data))
  data1 <- as.data.frame(data)
  leaflet(data = data) %>%
    addTiles() %>%  
    addMarkers(lng=data$LONGITUDE, lat=data$LATITUDE, popup=as.character(data1[,a]))
}

#' Function that creates a popup text for each earthquake.
#'
#' This function generates a HTML formatted tag to be used in popups.
#'
#' @param data Clean earthquake data.
#' @return A character vector containing popup text to be used in a leaflet visualization.
#'
#' @examples
#' \dontrun{
#' file<-system.file("data","earthquakes_data.txt.zip",package="Capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(file))) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 1980) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data){
  data <- as.data.frame(data)
  contenido <- paste(sep ="<br/>",paste0("<b>","Location: ","</b>",data$LOCATION_NAME),
                     paste0("<b>","Magnitude: ","</b>",data$EQ_MAG_ML),
                     paste0("<b>","Total deaths: ","</b>",data$DEATHS))
  return(contenido)
}
