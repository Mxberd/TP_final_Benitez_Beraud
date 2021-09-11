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
library(osmdata)
library(sf)
library(ggmap)
library(leaflet)
library(datos)

#1 - **Una pregunta y presentacion del tema**

#1 - Queremos trabajar sobre el tema del arbolado urbano en la ciudad de Buenos Aires, particularmente 
#sobre la presencia de especies autoctonas significativas que enriquezcan la biodiversidad. 
#Sin embargo, dado que se trata de un ejercicio acotado, nos limitaremos a observar lo que sucede en el barrio
#de Palermo. PREGUNTA: Las especies autoctonas tienen una representacion significativa en el arbolado urbano?
#Se analizara el caso de Palermo.

#2 - **Descripcion de los pasos realizados**

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

#3 - **Importacion de datos a traves de scrapping**

#Hay mucha informacion de aficionados que se dedican a recopilar meticulosamente informacion
#Agradecemos esa vocacion de compartir.

#los paquetes rvest y tydiverse seran nuestros aliados en esta parte.

url <- "https://florabonaerense.blogspot.com/p/lista-de-plantas.html"

url_00 <- read_html(url)

#El paquete #read_html nos permite levantar los datos directamente de la web. 
# A partir de ahi arranca nuestra tarea de limpieza

Especies <- url_00 %>% 
  html_elements("span span") %>% 
  html_text2() %>% 
  data.frame(especies= c(.)) %>% 
  select(especies) 

#Proponemos generar mas y mejores datos uniendo otra informacion de otros sitios web,
#Especies hasta aqui tiene la mirada principalmente en arbustivas y arboles bajos.
#Necesitamos mas!

url_01<-"https://infoagro.com.ar/arboles-nativos-de-argentina-y-sudamerica/"
url_02 <- read_html(url_01)

Arboretum <- url_02 %>% 
  html_elements("p strong") %>% 
  html_text2() %>% 
  data.frame(especies= c(.)) %>% 
  select(especies) 


#4 - **Limpieza y tratamiento de datos**

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

#Usamos la combinacion de #case_when y de #str_remove para eliminar formulas como "Para jardines, 
#Para lugares soleados". La preposicion "Para" nos permite detectar esas formulas y luego eliminarlas
#Renombramos la columna como "especies nativas". #Evitemos confusiones.

Especies_01 <- Especies %>%
  separate(col = nombre_especie, into = c("nombre_vulgar", "nombre_cientifico"), sep = "\\(") %>% 
  filter (!is.na(nombre_cientifico))%>% 
  mutate(nombre_cientifico = str_remove(nombre_cientifico, str_c("\\)"))) %>% 
  separate(col = nombre_cientifico, into = c("nombre_cientifico","descripcion"), sep =" – ") %>% 
  select(-nombre_vulgar,-descripcion) 

#Usamos la funcion #separate para separar nombre vulgar de cientifico y nuevamente luego para separar
#Nombre de descripcion, que en la informacion extraida estaba mezclada.
#Filtramos los NAs con #!is.na
#Usamos #str_remove para terminar de eliminar los parentesis residuales de nuestro #Scrapping

#Procesamos tambien nuestra lista complementaria. #Slice #Rename #mutate #str_remove#if_else

Arboretum <- Arboretum %>%
  slice(2:8,10:16,18:18) %>%
  rename(nombre_cientifico="especies") %>%
  mutate(nombre_cientifico = str_remove(nombre_cientifico, str_c("\\:")),origen="nativa") %>% 
  mutate(nombre_cientifico = if_else( nombre_cientifico == "Chorisia speciosa-Ceiba speciosa",
  "Ceiba speciosa", nombre_cientifico))

#proponemos unir los datos 


Especies_02 <- Especies_01 %>% 
  add_case(Arboretum) %>% 
  unique()

#Primero unimos nuestras bases de datos de esepcies con #add_case y con #unique verificamos no haya repeticiones
  
#Ahora combinamos con #left_join la informacion del barrio!

Palermo_Repres_02 <- Palermo_Repres %>% 
  left_join(Especies_02)

#Si bien nuestra busqueda no fue exaustiva, si incluyo mas scrapping del que se ve plasmado en el ejercicio
#Por ello damos por concluida la busqueda de matcheos entre especies presentes en Palermo y la busqueda de la autentificacion
# de su condicion de nativas. Por esta razon se les asignara la etiqueda de "Exotica o no identificada" en relacion a su origen


Palermo_Repres_02 <- Palermo_Repres_02 %>% 
  mutate(origen=case_when(origen=="nativa"
  ~ origen, TRUE ~ "exotica o sin idenficiar"))

#Habiendo cerrado la etapa de clasificacion de clasificacion tenemos que volver a ligar esta informacion a 
#nuestros datos espaciales para avanzar con los mapeos de la etapa siguiente del ejercicio. #right_joint.

Arbolado_Palermo_02 <- Arbolado_Palermo %>% 
  right_join(Palermo_Repres_02)

#5 - Visualizaciones

#Primero proponemos un grafico sencillo para visualizar las especies presentes en Palermo
#Para este grafico observaremos solo las especies mas relevantes, de manera que ampliaremos aquellas que son irrelevantes
#y las descartaremos


ggplot(filter(Palermo_Repres_02, cantidad_registrada>50))+
  geom_bar(aes(x=reorder(nombre_cientifico, cantidad_registrada), weight=cantidad_registrada, fill=origen)) +
  labs(title="Arbolado, segun especies - Palermo, CABA",
  subtitle="comparacion relativa",
  fill="Origenes",
  x="especies",
  y="cantidad",
  caption="Fuente: BA DATA, infoargro, florabonaerense")+
  scale_fill_manual(values = c("goldenrod2", "indianred2"))+
  theme_light()+
  coord_flip()+
  theme_classic()

#Al haber generado el grafico de barras, pudimos identificar que la especie nativa mas presente
#es el Jacaranda o Jacaranda Mimosifolia y luego la Tipa o Tipuana Tipu. Entre las especies identificadas
#etre las primeras se ubicarian estas dos. De manera que hay cierta relevancia de las nativas en el arbolado en Palermo
#Pero Comparativamente con las dos primeras, Fresnos y Platanos, se evidencia que si representatividad es limitada.

#Sin embargo debemos analizar la distribucion de esos ejemplares, ya que podria haber diferencias sectoriales entre las distintas
#partes del barrio. 

#Para ello recurrimos a la library sf. Nececitamos hacer alguna conversion de formato con #st_as_sf

Arbolado_Palermo_03 <- Arbolado_Palermo_02 %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#transformas los datos espaciales en geometria sf y le incorporamos la proyeccion.

#Ahora vamos a producir un mapa usando las herramientas que nos prove OSMDATA y la library #Leaflet

bbox_Palermo<- getbb("Comuna 14,Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")

bbox_Palermo<- make_bbox(-34.5889, -58.4306)
mapa_Palermo <- get_stamenmap(bbox_Palermo,zoom = 12)

ggmap(mapa_Palermo)+
  geom_sf(mapping = Arbolado_Palermo_02, aes(x=lat, y=long), inherit.aes = FALSE)+
  labs(title="Arboles"
       subtitle="arboles"
       caption="BA data")+
  theme_void()


# Estamos listos para ubicar en ese mapa nuestros datos.

ggmap(Palermo_mapa)+
  geom_sf(data=Arbolado_Palermo_03, color=origen, size=1.5, inherit.aes = FALSE)
  
  
