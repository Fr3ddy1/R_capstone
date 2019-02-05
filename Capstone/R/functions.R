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
eq_clean_data <- function(data){
  #replace NA values
  data$MONTH[is.na(data$MONTH)] <- "01"
  data$DAY[is.na(data$DAY)] <- "01"
  
  #Find negative dates
  neg <- which(data$YEAR<0)
  neg1 <- as.Date(data$DATE[neg],format="%d/%m/-%Y")
  
  #find positives dates
  pos <- which(data$YEAR>0)
  pos1 <- as.Date(data$DATE[pos],format="%d/%m/%Y")
  
  #merga dates (positives and negatives)
  fechas <- c(neg1,pos1)
  
  #Add Date column
  data$DATE <- fechas
  
  #work with Longitude and Latitude column
  data$LATITUDE <- as.numeric(data$LATITUDE)
  
  data$LONGITUDE <- as.numeric(data$LONGITUDE)
  
  return(data)
  
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
eq_location_clean <- function(data){
  #find observations with ":"
  d <- c()
  
  for(i in 1:dim(data)[1]){
    d[i] <- length(gregexpr(pattern =':',data$LOCATION_NAME[i])[[1]])
  }
  
  #create vector with the new data location
  loc <- c()
  
  #separate diferents cases
  for(i in 1:dim(data)[1]){
    #local problem
    if(i==2027){
      loc[i] <- "NEW ZEALAND" 
    }else if(i==566 | i==1312 | i==2830 | i==3126 | i==5869){
      loc[i] <- str_remove(unlist(strsplit(data$LOCATION_NAME[i], split=':  ', fixed=TRUE))[2], ":")
      
    }else if(i==5917){
      loc[i] <- str_remove( unlist(strsplit(data$LOCATION_NAME[i], split=':', fixed=TRUE))[2], " ")
      
    }else{
      #one ":" case
      if(d[i]==1){
        # verify if exits one ":" or there is nothing
        c <- as.vector(gregexpr(pattern =':',data$LOCATION_NAME[i])[[1]])
        
        if(c==-1){
          #nothing to eliminate
          loc[i] <- data$LOCATION_NAME[i]
        }else{
          #one ":" case
          if(i==1492 | i==1506 | i==1705){
            #local problem
            loc[i] <-  str_remove(data$LOCATION_NAME[i], ":")
          }else{
            loc[i] <-  unlist(strsplit(data$LOCATION_NAME[i], split=':', fixed=TRUE))[2]
          }
        }
      }else if(d[i]==2){ #two ":" case
        e2 <- data$LOCATION_NAME[i]
        e3 <- str_remove(e2, ":")
        loc[i] <- unlist(strsplit(e3, split=':', fixed=TRUE))[2]
        
      }else if(d[i]==3){ #three ":" case
        g <- data$LOCATION_NAME[i]
        g1 <- str_remove(g, ":")
        g2 <- str_remove(g1, ":")
        loc[i] <- unlist(strsplit(g2, split=':', fixed=TRUE))[2]
        
      }
    }#final if 
    
  }#final for creation new col location_name
  
  #last step
  loc1 <- toTitleCase(tolower(loc))
  return(loc1)
}

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



#creo funcion
eq_map <- function(data,annot_col){
  a <- which(annot_col==names(data))
  data1 <- as.data.frame(data)
  leaflet(data = data) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=data$LONGITUDE, lat=data$LATITUDE, popup=as.character(data1[,a]))
  
}

#funcion pedida
eq_create_label <- function(data){
  data <- as.data.frame(data)
  contenido <- paste(sep ="<br/>",paste0("<b>","Location: ","</b>",data$LOCATION_NAME),
                     paste0("<b>","Magnitude: ","</b>",data$EQ_MAG_ML),
                     paste0("<b>","Total deaths: ","</b>",data$DEATHS))
  return(contenido)
}