#Tarea semana 4 - Depurada
library(readr)
library(geosphere)
library(tidyr)
library(magrittr)
library(ggplot2)

setwd("C:/Users/Freddy Tapia/Desktop/Scripts Trabajo")

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

#modifico data
#agrego columna "storm_id" 
#agrego columna "date"
library(magrittr)
a <- ext_tracks %>% dplyr::mutate(storm_id=paste(storm_name,year,sep = "-"),date=paste(paste(year,month,day,sep="-"),paste(hour,"00","00",sep = ":")))


#378 factores

a1 <- (levels(as.factor(a$storm_id)))

b <- a[which(a1[1]==a$storm_id),]

#caso Katrina
c <- a[which("KATRINA-2005"==a$storm_id),]

#31 valores
c1 <- levels(as.factor(c$date))

d <- c[which(c1[24]==c$date),]

e <- gather(d,TXTRadius, Distance, radius_34_ne:radius_64_nw, na.rm = TRUE)

f <- separate(e,TXTRadius, c("TXT1","wind_speed", "Orientation"), sep="_")

g <- spread(f,Orientation, Distance)

#data final
h <- g[,c(1,18,7,8,20,21,22,23,24)]

h$longitude <- -h$longitude

#creo funcion que me crea data a partir de la cual
#grafico los poligonos para las tres velocidades de viento

data_poligono <- function(name,dat){
  
  a <- dat %>% dplyr::mutate(storm_id=paste(storm_name,year,sep = "-"),date=paste(paste(year,month,day,sep="-"),paste(hour,"00","00",sep = ":")))
  
  
  c <- a[which(name==a$storm_id),]
  
  #58 valores
  c1 <- levels(as.factor(c$date))
  
  #guardo en una lista los resultados
  
  #View(c[which(c1[24]==c$date),])
  for (i in 1:length(c1)){
    
    if(i ==1){
      
      d <- c[which(c1[i]==c$date),]
      
      e <- gather(d,TXTRadius, Distance, radius_34_ne:radius_64_nw, na.rm = TRUE)
      
      
      f <- separate(e,TXTRadius, c("TXT1","wind_speed", "Orientation"), sep="_")
      
      g <- spread(f,Orientation, Distance)
      
      #data final
      H <- g[,c(1,18,7,8,20,21,22,23,24)]
      
      H$longitude <- -H$longitude
    }else{
      d <- c[which(c1[i]==c$date),]
      
      e <- gather(d,TXTRadius, Distance, radius_34_ne:radius_64_nw, na.rm = TRUE)
      
      
      f <- separate(e,TXTRadius, c("TXT1","wind_speed", "Orientation"), sep="_")
      
      g <- spread(f,Orientation, Distance)
      
      #data final
      h <- g[,c(1,18,7,8,20,21,22,23,24)]
      
      h$longitude <- -h$longitude
      
      H <- rbind.data.frame(H,h)
      
    } 
    
    #H[[i]] <- h
    
  }
  
  # H es la data con la cual trabajare
  return(H)
}#final funcion

#a partir de la data dt debo hacer una funcion que me 
#de un data frame para graficar los poligonos
bordes <- function(dt){
  
  #defino centro del huracan
  kat <- c(dt$longitude[1],dt$latitude[1])
  
  #34 nodos
  #creo semicirculo ne
  #distancia en metros
  circle_ne_34=destPoint(kat, b=0:90, d=dt$ne[1]*1852)
  
  #creo semicirculo nW
  circle_nw_34=destPoint(kat, b=270:360, d=dt$nw[1]*1852)
  
  #creo semicirculo SW
  circle_sw_34=destPoint(kat, b=180:270, d=dt$sw[1]*1852)
  
  #creo semicirculo SW
  circle_se_34=destPoint(kat, b=90:180, d=dt$se[1]*1852)
  
  #50 nodos
  #creo semicirculo ne
  circle_ne_50=destPoint(kat, b=0:90, d=dt$ne[2]*1852)
  
  #creo semicirculo nW
  circle_nw_50=destPoint(kat, b=270:360, d=dt$nw[2]*1852)
  
  #creo semicirculo SW
  circle_sw_50=destPoint(kat, b=180:270, d=dt$sw[2]*1852)
  
  #creo semicirculo SW
  circle_se_50=destPoint(kat, b=90:180, d=dt$se[2]*1852)
  
  #64 nodos
  #distancia en metros
  circle_ne_64=destPoint(kat, b=0:90, d=dt$ne[3]*1852)
  
  #creo semicirculo nW
  circle_nw_64=destPoint(kat, b=270:360, d=dt$nw[3]*1852)
  
  #creo semicirculo Sw
  circle_sw_64=destPoint(kat, b=180:270, d=dt$sw[3]*1852)
  
  #creo semicirculo SW
  circle_se_64=destPoint(kat, b=90:180, d=dt$se[3]*1852)
  
  #el orden importa!
  data_34 <- rbind.data.frame(circle_ne_34,circle_se_34,circle_sw_34,circle_nw_34)
  data_34$grupo <- "34"
  data_50 <- rbind.data.frame(circle_ne_50,circle_se_50,circle_sw_50,circle_nw_50)
  data_50$grupo <- "50"
  data_64 <- rbind.data.frame(circle_ne_64,circle_se_64,circle_sw_64,circle_nw_64)
  data_64$grupo <- "64"
  
  #uno datas
  data <- rbind.data.frame(data_34,data_50,data_64)
  
  return(data)
}#final funcion bordes

#creo poligono
#Ejemplo libro
data_katrina <- data_poligono("KATRINA-2005",ext_tracks)
#misma data de h y la del ejemplo del libro
dt <- data_katrina[which(data_katrina$date=="2005-08-29 12:00:00"),]
pol <- bordes(dt)

p <- ggplot(pol,aes(x=lon,y=lat,group = grupo,fill=grupo,color=grupo))+
  geom_polygon(alpha = 0.6)  +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow"))+
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
p

#caso tarea
data_ike <- data_poligono("IKE-2008",ext_tracks)

dt1 <- data_ike[which(data_ike$date=="2008-09-11 18:00:00"),]
pol1 <- bordes(dt1)

p1 <- ggplot(pol1,aes(x=lon,y=lat,group = grupo,fill=grupo,color=grupo))+
  geom_polygon(alpha = 0.6)  +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow"))+
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

p1

#monto grafico sobre el mapa
library(ggmap)
aa <- get_map("Louisiana", zoom = 6, maptype = "toner-background")

#
kat <- c(h$longitude[1],h$latitude[1])
#creo semicirculo ne
#distancia en metros
circle_ne=destPoint(kat, b=0:90, d=200*1852)

#creo semicirculo nW
#distancia en metros
circle_nw=destPoint(kat, b=270:360, d=100*1852)


#creo semicirculo SW
#distancia en metros
circle_sw=destPoint(kat, b=180:270, d=150*1852)

#creo semicirculo SW
#distancia en metros
circle_se=destPoint(kat, b=90:180, d=200*1852)

#50 nodos
#distancia en metros
circle_ne_50=destPoint(kat, b=0:90, d=120*1852)

#creo semicirculo nW
#distancia en metros
circle_nw_50=destPoint(kat, b=270:360, d=75*1852)

#creo semicirculo SW
#distancia en metros
circle_sw_50=destPoint(kat, b=180:270, d=75*1852)

#creo semicirculo SW
#distancia en metros
circle_se_50=destPoint(kat, b=90:180, d=120*1852)

#64 nodos
#distancia en metros
circle_ne_64=destPoint(kat, b=0:90, d=90*1852)

#creo semicirculo nW
#distancia en metros
circle_nw_64=destPoint(kat, b=270:360, d=60*1852)

#creo semicirculo SW
#distancia en metros
circle_sw_64=destPoint(kat, b=180:270, d=60*1852)

#creo semicirculo SW
#distancia en metros
circle_se_64=destPoint(kat, b=90:180, d=90*1852)
#
#el orden importa!
data_34 <- rbind.data.frame(circle_ne,circle_se,circle_sw,circle_nw)
data_34$grupo <- "34"
data_50 <- rbind.data.frame(circle_ne_50,circle_se_50,circle_sw_50,circle_nw_50)
data_50$grupo <- "50"
data_64 <- rbind.data.frame(circle_ne_64,circle_se_64,circle_sw_64,circle_nw_64)
data_64$grupo <- "64"

#uno datas
data <- rbind.data.frame(data_34,data_50,data_64)

#
aa %>% 
  ggmap(extent = "device") +
  geom_polygon(data=rbind.data.frame(kat,circle_ne_50),aes(x=lon,y=lat)) 

#
#replica muy similar al grafico del curso
aa %>% 
  ggmap(extent = "device") +
  geom_polygon(data=pol[which(pol$grupo==34),],aes(x=lon,y=lat),fill="red") + 
  geom_polygon(data=pol[which(pol$grupo==50),],aes(x=lon,y=lat),fill="orange")+
  geom_polygon(data=pol[which(pol$grupo==64),],aes(x=lon,y=lat),fill="yellow")

#
aa %>% 
  ggmap(extent = "device") +
  geom_polygon(data=data_34,aes(x=lon,y=lat),fill="red") + 
  geom_polygon(data=data_50,aes(x=lon,y=lat),fill="orange")+
  geom_polygon(data=data_64,aes(x=lon,y=lat),fill="yellow")

#prueba con la data unida
data3 <- data
data3$grupo <- as.factor(data3$grupo)

aa %>% 
  ggmap(extent = "device") +
  geom_polygon(data=data3,aes(x=lon,y=lat,group = grupo))  
  

#creo estructura para stat
StatPoligono <- ggproto("StatPoligono", Stat,
                   compute_group = bordes,
                   #default_aes = aes(),
                   required_aes = c("latitude", "longitude", "win_speed","ne","nw","se","sw"))

#defino el stat_
stat_poligono <- function(mapping = NULL, data = NULL, geom = "hurricane",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPoligono, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )


#creo estructura para geom
GeomHurricane <- ggproto("GeomHurricane ", Geom,
                         required_aes = c("lon","lat","grupo"),
                         #default_aes = aes(),
                         draw_key = draw_key_point,
                         draw_panel = draw_panel_function #importante
) 

#defino el geom_

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "poligono", 
                           position = "identity", show.legend = NA, 
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping,
    stat = stat,
    geom = GeomHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#codigo final q debe correr

# library(ggmap)
# get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#   ggmap(extent = "device") +
#   geom_hurricane(data = storm_observation,
#                  aes(x = longitude, y = latitude, 
#                      r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#                      fill = wind_speed, color = wind_speed)) + 
#   scale_color_manual(name = "Wind speed (kts)", 
#                      values = c("red", "orange", "yellow")) + 
#   scale_fill_manual(name = "Wind speed (kts)", 
#                     values = c("red", "orange", "yellow"))

#hay q generar un grob que se monte sobre el mapa a partir de una data como la del ejemplo
#de Katrina
