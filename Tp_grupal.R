# TP FINAL 
#Daiana Benitez, Max Beraud

#Consigna
#General: Realizar informe donde se pueda evidenciar el armado de un proyecto, pudiendo identficarse
#Etapas

#1 - Una pregunta y presentacion del tema

#2 - Descripcion de los pasos realizados

#3 - Importacion de datos a traves de scrapping

#4 - Limpieza y tratamiento de datos

#5 - Visualizacion 

#6 - Comentarios a modo de conclusion.


#Cargamos algunas librerias

library(rvest)
library(tidyverse)
library(tibble)
library(stringr)

#1 - Queremos trabajar sobre el tema del arbolado urbano en la ciudad de Buenos Aires, particularmente 
#sobre la presencia de especies autoctonas significativas que enriquezcan la biodiversidad. 
#Sin embargo, dado que se trata de un ejercicio acotado, nos limitaremos a observar lo que sucede en el barrio
#de Palermo. PREGUNTA: Las especies autoctonas tienen una representacion significativa en el arbolado urbano?
#Para ello nos basaremos en la regla de Santamour (1999). 

Arbolado <- read.csv("Datos/arbolado-publico-lineal-2017-2018.csv", stringsAsFactors= TRUE, encoding = "UTF-8", dec = ".") 

Arbolado_01 <- Arbolado%>% 
  filter(!comuna = 14)

Arbolado %>% tibble()

url <- "https://florabonaerense.blogspot.com/p/lista-de-plantas.html"

url_00 <- read_html(url)

Especies <- url_00 %>% 
  html_elements("span span") %>% 
  html_text2() 
Especies <- Especies %>% 
  data.frame(especies= c(.)) %>% 
  select(especies) %>%
  na.omit() %>% 
  slice(2:87)

tibble(Especies)  

#Trajo algunos elemenots indeseables. Los eliminamos

Especies <-Especies %>% 
  mutate(texto=case_when(str_detect( string = especies, pattern = "Para")
  ~ "Texto", TRUE ~ "Nombre de especie")) %>% 
  filter(!texto=="Texto") %>% 
  select(-texto)

Especies2 <-Especies %>%
  separate(especies, into = c("Nombre vulgar", "Nombre cientifico"), sep ="()") 


