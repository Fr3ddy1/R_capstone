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

