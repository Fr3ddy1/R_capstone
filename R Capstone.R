#R Capstone
#################################
#################################
#################################
########### SEMANA 1 ############
#################################
#################################
#################################

#leo la data
library(readr)
library(stringr)
library(tools)
library(ggplot2)

#signif_txt <- read_delim("Downloads/signif.txt.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
signif_txt <- read_delim(paste(getwd(),"signif.txt.tsv",sep = "/"),"\t", escape_double = FALSE, trim_ws = TRUE)


#columna data
as.Date("1/1/-2150",format="%d/%m/-%Y")

which(is.na(signif_txt$YEAR))

signif_txt$MONTH[is.na(signif_txt$MONTH)] <- "01"
signif_txt$DAY[is.na(signif_txt$DAY)] <- "01"

signif_txt$DATE <- paste(signif_txt$DAY,signif_txt$MONTH,signif_txt$YEAR,sep = "/")

#busco los de fecha negativa
neg <- which(signif_txt$YEAR<0)
neg1 <- as.Date(signif_txt$DATE[neg],format="%d/%m/-%Y")

#los no neg son
pos <- which(signif_txt$YEAR>0)
pos1 <- as.Date(signif_txt$DATE[pos],format="%d/%m/%Y")

#uno fechas
fechas <- c(neg1,pos1)


#1) FECHAS
signif_txt$DATE <- fechas


###############
#2) convertir latitud y longitud a clase numerica


#crear funcion eq_location_clean que modifica la columna Location_name
#eliminar nombre del pais y los dos puntos
#y pasar todas las letras mayusculas a minusculas

#----------------------------d
head(signif_txt$LOCATION_NAME)
tail(signif_txt$LOCATION_NAME)

#ojo con 994 tiene dos : LUZON

a <- signif_txt$LOCATION_NAME[1]

# a1 <- gregexpr(pattern =':',a)
# 
# #posicion donde esta los dos puntos
# a1[[1]][1]
# seq(1,a1[[1]][1])
# 
# #ejemplo
# s = "TGAS_1121"
# s1 = unlist(strsplit(s, split='_', fixed=TRUE))[2]
# s1    

#
s1 = unlist(strsplit(a, split=':  ', fixed=TRUE))[2]
s1  

# a2 <- gregexpr(pattern ='  ',s1)
# 
# #posicion donde esta los dos espacios en blanco
# a2[[1]][1]

#
# s2 = unlist(strsplit(s1, split='  ', fixed=TRUE))[2]
# s2  

#problemas
b <- signif_txt$LOCATION_NAME[994]

b1 = unlist(strsplit(b, split=':  ', fixed=TRUE))[2]
b1  

c <- gregexpr(pattern =':',b)
c[[1]][1]

#indicador de que tiene dos puntos
length(c[[1]])

d <- c()
d1 <- c()

for(i in 1:dim(signif_txt)[1]){
d[i] <- length(gregexpr(pattern =':',signif_txt$LOCATION_NAME[i])[[1]])

# if(d[i]==1 & gregexpr(pattern =':',signif_txt$LOCATION_NAME[i])[[1]]==-1){
#   d1[i] <- i
# }else{
#   d1[i] <- 0
# }

}

length(which(d==3))
head(signif_txt$LOCATION_NAME[which(d==3)])
length(which(d==2))
head(signif_txt$LOCATION_NAME[which(d==2)])

length(which(d==1))

#hay q hacer 3 casos uno para un solo ":", otro para dos ":" y otro pa tres
#":"

#caso un ":"
e1 = unlist(strsplit(signif_txt$LOCATION_NAME[1], split=':  ', fixed=TRUE))[2]
e1

#caso dos ":"
e2 <- signif_txt$LOCATION_NAME[9]
e3 <- str_remove(e2, ":")

f1 = unlist(strsplit(e3, split=':  ', fixed=TRUE))[2]
f1

#caso tres ":"
#ojo pensar si tiene sentido esto
g <- signif_txt$LOCATION_NAME[197]
g1 <- str_remove(g, ":")
g2 <- str_remove(g1, ":")


g3 = unlist(strsplit(g2, split=':  ', fixed=TRUE))[2]
g3

###############d
#----------------------------d

#creo funcion 
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


#reviso na
nas <- is.na(loc)
nas1 <- which(nas)

signif_txt$LOCATION_NAME[nas1]

#problema existen dos puntos
b <- signif_txt$LOCATION_NAME[5859]

b1 = unlist(strsplit(b, split=':  ', fixed=TRUE))[2]
b2 <- str_remove(b1, ":")
b2  

c <- gregexpr(pattern =':',b)
c[[1]][1]==-1

#caso 5917
b <- signif_txt$LOCATION_NAME[5917]

b1 = unlist(strsplit(b, split=':', fixed=TRUE))[2]
b2 <- str_remove(b1, " ")
b2  


#vector que me indica q hay un ":" o no hay
d1 <- which(d==1)
d2 <- c()

for(i in 1:length(d1)){
  d2[i] <- gregexpr(pattern =':',signif_txt$LOCATION_NAME[i])[[1]]
}


#estos son los 9 casos donde hay problemas
View(signif_txt[nas1,])

#convierto vector loc en minuscula y luego a title case
loc1 <- toTitleCase(tolower(loc))

###############
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


#
sig <- eq_location_clean(signif_txt)

signif_txt$LOCATION_NAME <- sig

#################################
#################################
#################################
########### SEMANA 2 ############
#################################
#################################
#################################

#generar geom_timeline() 
#debe generar un grafico donde el eje de las x son las fechas
#y su alturas pueden ser uno o varios paises
#se debe incluir escala de Richter y numero de muertes

#ML magnitud es la escala de Richter
summary(signif_txt$EQ_MAG_ML)
summary(signif_txt$TOTAL_DEATHS)

#variables importantes
#Country: Pais
#Location_name: ubicacion
#eq_mag_ml: escala de Richter
#Total_deaths: numero de muertes

#EJEMPLO AFGANISTAN

signif_txt$COUNTRY <- as.factor(signif_txt$COUNTRY)

levels(signif_txt$COUNTRY)[1]

af <- signif_txt[which(levels(signif_txt$COUNTRY)[1]==signif_txt$COUNTRY),]

#escala de Richter
af$EQ_MAG_ML

#numero de muertes
af$TOTAL_DEATHS


##### Ejemplo geom #####z
library(grid)
library(faraway)
GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 1),
                       draw_key = draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)
                         
                         ## Let's print out the structure of the 'coords' object
                         str(coords)
                         
                         ## Construct a grid grob
                         pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape
                         )
                       })

#
geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyPoint, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
ggplot(data = worldcup, aes(Time, Shots)) + geom_mypoint()

##### Final Ejemplo geom #####z

#ejemplo con Afganistan
#toda la data
names(af)

ggplot(data = af,aes(x=DATE,y=rep(1,nrow(af)),size=EQ_MAG_MS)) + geom_point(alpha=0.3,na.rm = TRUE)

ggplot(data = af,aes(x=DATE,y=rep(1,nrow(af)),size=TOTAL_DEATHS)) + geom_point(alpha=0.3,na.rm = TRUE)

ggplot(data = af,aes(x=DATE,y=rep(1,nrow(af)))) + geom_point(aes(size=TOTAL_DEATHS),color="red",alpha=0.3,na.rm = TRUE,show.legend = TRUE) +
  geom_point(aes(size=EQ_MAG_MS),alpha=0.3,na.rm = TRUE,show.legend = TRUE)



#prueba con data sin NA
#para numero de muertes
af1 <- af[-which(is.na(af$TOTAL_DEATHS)),]

g1 <- ggplot(data = af1,aes(x=DATE,y=rep(1,nrow(af1)),size=TOTAL_DEATHS)) + geom_point(color="blue")

#para scala de Richter
af2 <- af[-which(is.na(af$EQ_MAG_MS)),]

g2 <- ggplot(data = af2,aes(x=DATE,y=rep(1,nrow(af2)),size=EQ_MAG_MS)) + geom_point(alpha=0.3)

g1+geom_point(data = af2,aes(x=DATE,y=rep(1,nrow(af2)),size=EQ_MAG_MS))

#######
#AYUDA
library(ggplot2)
library(dplyr)
library(devtools)
library(relayer) # devtools::install_github("clauswilke/relayer")

# make aesthetics aware size scale, also use better scaling
scale_size_c <- function(name = waiver(), breaks = waiver(), labels = waiver(), 
                         limits = NULL, range = c(1, 6), trans = "identity", guide = "legend", aesthetics = "size") 
{
  continuous_scale(aesthetics, "area", scales::rescale_pal(range), name = name, 
                   breaks = breaks, labels = labels, limits = limits, trans = trans, 
                   guide = guide)
}


lev <- c("A", "B", "C", "D")

nodes <- data.frame(
  ord = c(1,1,1,2,2,3,3,4),
  brand = factor(c("A", "B", "C", "B", "C", "D", "B", "D"), levels=lev), 
  thick = c(16, 9, 9, 16, 4, 1, 4, 1)
)

edge <- data.frame(
  ord1 = c(1, 1, 2, 3),
  brand1 = factor(c("C", "A", "B", "B"), levels = lev),
  ord2 = c(2, 2, 3, 4),
  brand2 = c("C", "B", "B", "D"),
  N1 = c(2, 1, 2, 1),
  N2 = c(5, 5, 2, 1)
)

ggplot() + 
  (geom_segment(
    data = edge,
    aes(x = ord1, y = brand1, xend = ord2, yend = brand2, edge_size = N2/N1), 
    color = "blue"
  ) %>% rename_geom_aes(new_aes = c("size" = "edge_size"))) +
  (geom_point(
    data = nodes,
    aes(x = ord, y = brand, node_size = thick),
    color = "black", shape = 16
  ) %>% rename_geom_aes(new_aes = c("size" = "node_size"))) + 
  scale_x_continuous(
    limits = c(1, 4),
    breaks = 0:4,
    minor_breaks = NULL
  ) +
  scale_size_c(
    aesthetics = "edge_size",
    breaks = 1:5,
    name = "edge size",
    guide = guide_legend(keywidth = grid::unit(1.2, "cm"))
  )  + 
  scale_size_c(
    aesthetics = "node_size",
    trans = "sqrt",
    breaks = c(1, 4, 9, 16),
    name = "node size"
  )  + 
  ylim(lev) + theme_bw()

#OTRA MANERA
ggplot(mtcars, aes(x = disp
                   , y = mpg)) +
  ##region for high mpg 
  geom_rect(aes(linetype = "High MPG")
            , xmin = min(mtcars$disp)-5
            , ymax = max(mtcars$mpg) + 2
            , fill = NA
            , xmax = mean(range(mtcars$disp))
            , ymin = 25
            , col = "black") + 
  ## test diff region
  geom_rect(aes(linetype = "Other Region")
            , xmin = 300
            , xmax = 400
            , ymax = 30
            , ymin = 25
            , fill = NA
            , col = "black") +
  geom_point(aes(col = factor(cyl)
                 , shape = factor(vs))
             , size = 3) +
  scale_color_brewer(name = "Cylinders"
                     , palette = "Set1") +
  scale_shape(name = "V/S") +
  scale_linetype_manual(values = c("High MPG" = "dotted"
                                   , "Other Region" = "dashed")
                        , name = "Region")

#OTRA OPCION
d <- data.frame(Number = rnorm(12,100,20), 
                Treatment = rep(c("A","B","C", "D"), each = 3))
av <- aggregate(d["Number"], d["Treatment"], mean)

ggplot(data = d, aes(y = Number, x = Treatment)) + 
  geom_point(shape = 1, size = 6,  color = "grey50") +
  geom_point(data=av, shape = 4) +
  theme_bw()

#
ggplot(data = d, aes(y = Number, x = Treatment)) + 
  geom_point(aes(shape = "1", size = "6",  color = "grey50")) +
  geom_point(data=av, aes(shape = "4")) +
  theme_bw() +
  scale_shape_manual(name = "", values = c(1,4), labels = c("observed values", "mean")) +
  scale_size_manual(name = "", values = c(6,1), labels = c("observed values", "mean")) +
  scale_color_manual(name = "", values = c("grey50","black"), 
                     labels = c("observed values", "mean")) +
  theme(legend.position = "top",
        legend.key = element_rect(color = NA))

#

av$Aggregated <- "mean"
d$Aggregated <- "observed value"
d <- rbind(d, av)

ggplot(data = d, aes(y = Number, x = Treatment, 
                     shape=Aggregated, size=Aggregated, colour=Aggregated)) + 
  geom_point() 



#
#
#probar esta!!
ggplot(mtcars, aes(x = disp
                   , y = mpg)) +
  geom_point(aes(col = factor(cyl)
                 , shape = factor(vs))
             , size = 3) +
  scale_color_brewer(name = "Cylinders") +
  scale_shape(name = "V/S") +
  theme(legend.position = "bottom")
  

#GRAFICO OBTENIDO MUY PARECIDO AL DE COURSERA
  ggplot(data = af,aes(x=DATE,y=rep(1,nrow(af)))) + 
  geom_point(aes(size=(EQ_MAG_MS),col=(TOTAL_DEATHS)),alpha=0.3,na.rm = TRUE,show.legend = TRUE) +
    geom_hline(yintercept=1,color = "gray")+
    #geom_text(aes(x=DATE, label = LOCATION_NAME, size=3, col="white"))+
    annotate("text", x = af$DATE[10], y = 1.001,
             label = af$LOCATION_NAME[10], parse = TRUE)+
    annotate("segment", x = af$DATE[10], xend = af$DATE[10], y = 1, yend = 1.0009,
             colour = "gray")+
    
    theme_classic()+
    theme(legend.position = "bottom",axis.title.y=element_blank(),
          panel.grid = element_blank(),
          #axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.background = element_blank()) 

  #AHORA SE DEBE COLOCAR NOMBRES A LOS PUNTOS DE MAYOR IMPORTANCIA
  #TANTOS COMO ESPECIFIQUE EL USUARIO "NMAX"
  #DEBO COLOCAR LOCATION_NAME
  
  #af ordenada por EQ_MAG_MS
  af1 <- af[order(af$EQ_MAG_MS,decreasing = TRUE),]
  #quito NA
  af1 <- af1[-which(is.na(af1$EQ_MAG_MS)),]
  
  #eljio 6 primeros , este valor debe ser reactivo
  head(af1$LOCATION_NAME)

    
##DEFINO GEOM
  GeomTimeline <- ggproto("GeomTimeline", Geom,
                         required_aes = c("x", "y","size"),
                         default_aes = aes(shape = 1,size=1, colour = "black"),
                         draw_key = draw_key_point,
                         draw_panel = function(data, panel_scales, coord) {
                           ## Transform the data first
                           coords <- coord$transform(data, panel_scales)
                           
                           ## Let's print out the structure of the 'coords' object
                           str(coords)
                           
                           ## Construct a grid grob
                           pointsGrob(
                             x = coords$x,
                             y = coords$y,
                             pch = coords$shape,
                             size = unit(coords$size,"char"),
                             gp = grid::gpar(col = coords$colour)
                           )
                           
                         })
  
  #
  geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = TRUE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomTimeline, mapping = mapping,  
      data = data, stat = stat, position = position, 
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
  
  #
  ggplot(data = af, aes(x=DATE,y=rep(1,nrow(af)))) + geom_timeline(shape=1,size=af$EQ_MAG_MS,color=af$TOTAL_DEATHS)
  

