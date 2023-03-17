#############CLASE-6##########
rm(list=ls())
# Configurar el directorio de trabajo
directorio <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directorio)

# Definamos un directorio para guardar los output de mi analisis:
output <- paste0(getwd(), "/output/")

### Configuraciones iniciales ###
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)
library(ggrepel)


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

# Carguemos el mapa 
departamental <- read_sf("C:\\Users\\leona\\Desktop\\CURSO R\\CLASE.08\\CLASE.08\\mapaDpto\\DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")


## GENERACIÓN DE GRÁFICOS ##

# Número de valores faltantes por columna
colSums(is.na(df))

## Gráfico 1:
graf1 <- df %>% 
  # Transformemos la columna EDAD
  mutate(EDAD = as.numeric(EDAD)) %>% ggplot(aes(x=EDAD, fill=SEXO))+ 
  geom_histogram(stat = "count")+scale_fill_manual(values = c("#EF0C58","#2284B2"))+
  labs(y="Cantidad de Fallecidos", x="Edad de Fallecidos", title = "Histograma de la edad de los fallecidos",
       caption="Fuente: Portal de Datos Abiertos")+
  # Separando por SEXO
  facet_grid(SEXO~.) 

graf1 <- grid.arrange(graf1, bottom= textGrob("SINADEF", x=0.9, hjust = 1,
                                              gp = gpar(fontface="italic", fontsize=14)))

# Guardemos las salidas de nuestro directorio output:
ggsave(filename = paste0(output, "HistogramaEDAD_SEXO.png"), width = 6, height = 10)


## Gráfico 2: 
graf2 <- df %>% count(FECHA, SEXO) %>% ggplot(aes(x=FECHA, y=n))+geom_line(aes(col=SEXO))+
  scale_x_date(date_labels = "%b")+labs(y= "Número de fallecidos", x="")+
  # Agreguemos una capa theme para modificar la posicion de la leyenda
  theme(legend.position = "bottom", legend.title = element_blank())

graf2

## Gráfico 3: 
graf3 <- df %>% mutate(EDAD=as.numeric(EDAD)) %>% ggplot(aes(x= SEXO, y=EDAD))+
  geom_violin(scale = "count", aes(fill= SEXO))

graf3

## Actualicemos df considerando solo los dptos del Perú 
df <- df %>% filter(DEPARTAMENTO.DOMICILIO %in% departamental$NOMBDEP)

## Gráfico 4: 
graf4 <- departamental %>% 
  left_join(df %>% count(DEPARTAMENTO.DOMICILIO, name ="DECESOS"),
                        by= c("NOMBDEP"="DEPARTAMENTO.DOMICILIO")) %>% ggplot()+
  geom_sf(aes(fill= DECESOS), show.legend= T, colour= "white")+
  geom_label_repel(aes(label= NOMBDEP, geometry=geometry), size=2, 
  stat="sf_coordinates", min.segment.length = 0, label.size = 1)+
  scale_fill_viridis_c(trans="sqrt", alpha = 0.4)+theme_void()

ggsave(filename = paste0(output, "NumFallecidos.png"), width = 6, height = 10) 

graf4

colnames(departamental)
colnames(df)

## Gráfico 5: 
tmp <- departamental %>% 
  left_join(df %>% count(DEPARTAMENTO.DOMICILIO, name ="DECESOS"),
            by= c("NOMBDEP"="DEPARTAMENTO.DOMICILIO")) %>% 
  mutate(DECESOS = as.numeric(DECESOS))

colnames(tmp)

graf5 <- tmp %>% ggplot()+
  geom_sf(aes(fill= DECESOS), show.legend= T, colour= "white")+
  geom_label_repel(data= tmp %>% filter(DECESOS >= 60000) ,
  ## SOLO MOSTRAR EL DEPARTAMENTO CON >= 60 000 DECESOS
                   aes(label= NOMBDEP, geometry=geometry), size=2, 
                   stat="sf_coordinates", min.segment.length = 0, label.size = 1)+
  scale_fill_viridis_c(trans="sqrt", alpha = 0.4)+theme_void()

graf5




