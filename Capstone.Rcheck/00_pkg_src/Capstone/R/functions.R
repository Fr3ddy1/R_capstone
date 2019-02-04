#leo data
eq_data_read <- function(filename) {
  
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(filename, delim='\t',progress = FALSE)
  })
  dplyr::tbl_df(data)
  
}


#FUNCION eq_clean_data
eq_clean_data <- function(data){
  #añado columna fecha
  data$MONTH[is.na(data$MONTH)] <- "01"
  data$DAY[is.na(data$DAY)] <- "01"
  
  #busco los de fecha negativa
  neg <- which(data$YEAR<0)
  neg1 <- as.Date(data$DATE[neg],format="%d/%m/-%Y")
  
  #los no neg son
  pos <- which(data$YEAR>0)
  pos1 <- as.Date(data$DATE[pos],format="%d/%m/%Y")
  
  #uno fechas
  fechas <- c(neg1,pos1)
  
  #FECHAS
  data$DATE <- fechas
  
  #lat y lon
  data$LATITUDE <- as.numeric(data$LATITUDE)
  
  data$LONGITUDE <- as.numeric(data$LONGITUDE)
  
  return(data)
  
}

#3) CREO FUNCION eq_location_clean

eq_location_clean <- function(signif_txt){
  #creo vector d con el fin de saber en cuales observaciones hay uno o varios 
  #":"
  d <- c()
  
  for(i in 1:dim(signif_txt)[1]){
    d[i] <- length(gregexpr(pattern =':',signif_txt$LOCATION_NAME[i])[[1]])
  }
  
  #
  #uso el vector d que me perimite saber q caso aplico
  loc <- c()
  
  
  for(i in 1:dim(signif_txt)[1]){
    #condicion de obs 2027
    if(i==2027){
      loc[i] <- "NEW ZEALAND" 
    }else if(i==566 | i==1312 | i==2830 | i==3126 | i==5869){
      loc[i] <- str_remove(unlist(strsplit(signif_txt$LOCATION_NAME[i], split=':  ', fixed=TRUE))[2], ":")
      
    }else if(i==5917){
      loc[i] <- str_remove( unlist(strsplit(signif_txt$LOCATION_NAME[i], split=':', fixed=TRUE))[2], " ")
      
    }else{
      #caso de un ":"
      if(d[i]==1){
        #en este caso puede ocurrir q tenga un ":" o no tenga nada
        #verifico esto
        c <- as.vector(gregexpr(pattern =':',signif_txt$LOCATION_NAME[i])[[1]])
        
        if(c==-1){
          #en este caso no hago nada, no hay nada q eliminar
          loc[i] <- signif_txt$LOCATION_NAME[i]
        }else{
          #en este caso hay un ":" y procedo con normalidad
          #condicional para arreglar problema en obs 1492, 1506 y 1705
          if(i==1492 | i==1506 | i==1705){
            loc[i] <-  str_remove(signif_txt$LOCATION_NAME[i], ":")
          }else{
            loc[i] <-  unlist(strsplit(signif_txt$LOCATION_NAME[i], split=':', fixed=TRUE))[2]
          }
        }
      }else if(d[i]==2){ #caso dos ":"
        e2 <- signif_txt$LOCATION_NAME[i]
        e3 <- str_remove(e2, ":")
        loc[i] <- unlist(strsplit(e3, split=':', fixed=TRUE))[2]
        
      }else if(d[i]==3){ #caso tres ":"
        g <- signif_txt$LOCATION_NAME[i]
        g1 <- str_remove(g, ":")
        g2 <- str_remove(g1, ":")
        loc[i] <- unlist(strsplit(g2, split=':', fixed=TRUE))[2]
        
      }
    }#final if obs 2027
    
  }#final for creacion nueva col location_name
  
  #ultimo paso
  #convierto vector loc en minuscula y luego a title case
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