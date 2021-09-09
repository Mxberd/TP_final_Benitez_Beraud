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
library(glue)

#1 - Queremos trabajar sobre el tema del arbolado urbano en la ciudad de Buenos Aires, particularmente 
#sobre la presencia de especies autoctonas significativas que enriquezcan la biodiversidad. 
#Sin embargo, dado que se trata de un ejercicio acotado, nos limitaremos a observar lo que sucede en el barrio
#de Palermo. PREGUNTA: Las especies autoctonas tienen una representacion significativa en el arbolado urbano?
#Se analizara el caso de Palermo.

#2 - Descripcion de los pasos realizados

#empezamos trayendo un dataset de BADATA, el cual fue simplicado eliminando NAS y columnas innecesarias
#Habia que aligerar el archivo base. #De todos modos esta el Zip cargado tambien en github si hubiera que descargarlo.

Arbolado <- read.csv("Datos/arbolado-publico-lineal-2017-2018.csv", stringsAsFactors= TRUE, encoding = "UTF-8", dec = ".") %>% 
  filter(!is.na(long)) %>%
  select(-manzana,-nro_registro,-tipo_activ,-calle_altura,-direccion_normalizada,-ubicacion,-ancho_acera,-estado_plantera,-ubicacion_plantera,-nivel_plantera,-diametro_altura_pecho,-altura_arbol)

#Volvemos a cargar nuestro nuevo csv que generamos a partir de "write.csv"

write.csv(Arbolado, file = "arbolado-2017-2018.csv",row.names = F)

#Este nuevo archivo es mas liviando y manejable, lo volvemos a importar.
#Cambiamos nombres que nos resultan anti-intuitivos.
#Eliminamos posibles repeticiones de carga de datos.

Arbolado_00 <-read.csv("Datos/arbolado-2017-2018.csv") %>% 
  rename("Calle"=calle_nombre, "Altura"=calle_chapa) %>% 
  unique()

#Filtramos nuestros datos correspondientes a la comuna 14 (Palermo)

Arbolado_Palermo <- Arbolado_00%>% 
  filter(comuna == '14')

#Proponemos contabilizar las especies detectadas, asi podremos tambien identificarlas y conocer su representatividad.
#Las ordenamos de manera decreciente.
Palermo_Repres<- Arbolado_Palermo  %>% 
  select(nombre_cientifico)%>%
  group_by(nombre_cientifico) %>% 
  summarise(cantidad_registrada=n()) %>% 
  arrange(desc(cantidad_registrada)) %>% 
  mutate(especie_relevante=case_when(cantidad_registrada<2
  ~ "No_relevante", TRUE ~ "Relevante"))

#Ya hemos realizado una primera limpieza parcial de nuestros datos (adelantandonos a #4)
#Tambien etiquetamos como no relevantes a las especies escasamente representadas, 
#1 ejemplar sobre los 15704 de Palermo no es signficativo!

#Sin embargo , para poder avanzar, desconocemos cuales de esas especies son efectivamente nativas y cuales no
#Usaremos la tecnica de scrapping para hacernos de esa informacion!

#3 - Importacion de datos a traves de scrapping

#Hay mucha informacion de aficionados que se dedican a recopilar meticulosamente informacion
#Agradecemos esa vocacion de compartir.

#los paquetes rvest y tydiverse seran nuestros aliados en esta parte.

url <- "https://florabonaerense.blogspot.com/p/lista-de-plantas.html"

url_00 <- read_html(url)

Especies <- url_00 %>% 
  html_elements("span span") %>% 
  html_text2() %>% 
  data.frame(especies= c(.)) %>% 
  select(especies) 

#Extrajimos con el inspector gadget el cuadro donde contenia la informacion de las especies
#Nativas, Pero este trajo algunas cosas de mas que debemos eliminar para continuar.

Especies <- Especies %>% 
  slice(2:87)%>% 
  mutate(texto=case_when(str_detect( string = especies, pattern = "Para")
  ~ "Texto", TRUE ~ "Nombre de especie")) %>% 
  filter(!texto=="Texto") %>% 
  select(-texto) %>% 
  rename(nombre_especie=especies) %>% 
  mutate(origen="nativa")

#Usamos la combinacion de case_when y de str_detect para eliminar formulas como "Para jardines, 
#Para lugares soleados". La preposicion "Para" nos permite detectar esas formulas y luego eliminarlas
#Renombramos la columna como "especies nativas". #Evitemos confusiones.

