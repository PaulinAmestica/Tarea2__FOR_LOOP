# Se le entregan 12 bases de datos referentes a caracteristicas de empresas de los paises de
# Chile, Colombia y Peru. Las bases de datos estan dentro de los periodos 2012 al 2017, y el
# nombre que tienen asociado esta compuesto por el tamanio de las empresas y su
# procedencia.

# 1. Cargar bases de datos e incorporar variable "tamanio"

################################ CARGA BASE DE DATOS ####################################### 
# install.packages("readr")
library(readr)
grandes_chile <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/grandes_chile.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
grandes_colombia <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/grandes_colombia.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
grandes_peru <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/grandes_peru.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
medianas_chile <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/medianas_chile.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
medianas_colombia <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/medianas_colombia.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
medianas_peru <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/medianas_peru.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
pequena_chile <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/pequena_chile.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
pequena_colombia <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/pequena_colombia.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
pequena_peru <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/pequena_peru.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
micro_chile <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/micro_chile.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
micro_colombia <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/micro_colombia.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
micro_peru <- read_delim("C:/Users/Alumno/Desktop/Trabajo de bigdata/micro_peru.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)


########################## INCORPORACION DE VARIABLE "TAMANIO" #################################

# install.packages("dplyr")
library(dplyr)
chileBase1 <- mutate(grandes_chile,tamanio = "Grande empresa")
chileBase2 <- mutate(medianas_chile,tamanio = "Mediana empresa")
chileBase3 <- mutate(pequena_chile,tamanio = "Pequenia empresa")
chileBase4 <- mutate(micro_chile,tamanio = "Micro empresa")

colombiaBase1 <- mutate(grandes_colombia,tamanio = "Grande empresa")
colombiaBase2 <- mutate(medianas_colombia,tamanio = "Mediana empresa")
colombiaBase3 <- mutate(pequena_colombia,tamanio = "Pequenia empresa")
colombiaBase4 <- mutate(micro_colombia,tamanio = "Micro empresa")

peruBase1 <- mutate(grandes_peru,tamanio = "Grande empresa")
peruBase2 <- mutate(medianas_peru,tamanio = "Mediana empresa")
peruBase3 <- mutate(pequena_peru,tamanio = "Pequenia empresa")
peruBase4 <- mutate(micro_peru,tamanio = "Micro empresa")




# 2. Reunir todas las bases en una sola, defina de que tipología 
# (tipo de datos) son cada una de las variables


chileTotalDatos <-   bind_rows(chileBase1,chileBase2,chileBase3, chileBase4)[1:13]
colombiaTotalDatos <-  bind_rows(colombiaBase1,colombiaBase2,colombiaBase3, colombiaBase4)[1:13]
peruTotalDatos <-  bind_rows(peruBase1, peruBase2, peruBase3, peruBase4)[1:13]

muestraParcial <-  rbind(chileTotalDatos, colombiaTotalDatos)


granBaseDeDatos <- bind_rows(peruTotalDatos,muestraParcial)[1:13]





# 3. Determine a traves del uso de condicionales y/o for cuantas observaciones 
# tiene Peru versus Chile


for(x in granBaseDeDatos[2]){
   print(paste("En la base de datos existen", sum(granBaseDeDatos[2] == "chile"), 
               "observaciones que corresponden a Chile"))
   print(paste("A su vez, la base de datos tambien contiene",sum(granBaseDeDatos[2] == "peru"),
               "observaciones que corresponden a Peru"))
}




# 4.  Determine a traves del uso de condicionales y/o for 
# cual es el pais con mayor ingresos de explotacion para los anios 
# que considera la muestra.


ingChile <- print(sum(chileTotalDatos[3])/nrow(chileTotalDatos[3]))
ingColombia <- print(sum(colombiaTotalDatos[3])/nrow(colombiaTotalDatos[3]))
ingPeru <- print(sum(peruTotalDatos[3])/nrow(peruTotalDatos[3]))


if(ingChile>ingColombia && ingChile>ingPromPeru){
  print(paste("Chile tiene mayor ing.de explotacion en periodo 2012-2017, en promedio:",
              ingChile))}else{
                if(ingColombia>ingChile && ingColombia>ingPeru){
                  print(paste("Colombia tiene mayor ing.de explotacion en periodo 2012-2017, en promedio:", 
                              ingColombia))}else{
                                   if(ingPeru>ingChile && ingPeru>ingColombia){
                                     print(paste("Peru tiene mayor ing.de explotacion en periodo 2012-2017, en promedio:",
                                                 ingPeru))
                                   }
                                 }
              }



# 7. Grafique algunas variables seleccionadas, las cuales 
# puedan responder a una pregunta que se haga con respecto a los datos.

# Cuales son las empresas que han realizado mas y menos importaciones, en cada pais?


# install.packages("ggplot2")
library(ggplot2)

ggplot(granBaseDeDatos, aes(fill=tamanio, y=importaciones,x=pais)) + 
   geom_bar(position = "dodge", stat = "identity") + theme_minimal()


# Represente graficamente el nivel de costos de las empresas Chilenas  y determine que tipo de empresas tiene mayor nivel de costos.

ggplot(chileTotalDatos, aes(x="", y=costos, fill=tamanio))+
   geom_bar(stat = "identity",width = 1)+
   coord_polar("y", start = 0)
