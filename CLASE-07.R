#############CLASE-6##########
rm(list=ls())
# Configurar el directorio de trabajo
directorio <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)

### Configuraciones iniciales ###
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)

## Carguemos los datos de sinadef
df <- read.csv("fallecidos_sinadef.csv", sep = "|")

# Carguemos el diccionario
dict <- read_xlsx("Diccionario_Datos_SINADEF.xlsx")

# Nombres de las Columnas
colnames(df)

# Variable FECHA
str(df)

## Transformemos la columna FECHA a un Date
df$FECHA <- as.Date(df$FECHA, format="%Y-%m-%d")
head(df$FECHA)

# Posibles valores de la columno AÑO
sort(unique(df$AÑO))

# Posibles valores de la columna SEXO
unique(df$SEXO)

# Filtremos la información para el año 2020 considerando solo los sexos M y F
df <- df %>% filter(FECHA >= as.Date("2020-01-01") & FECHA <= as.Date("2020-12-31"),
                    SEXO %in% c("FEMENINO", "MASCULINO")) 










  
