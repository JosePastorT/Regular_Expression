
#--------------- Regular Expressions ---------------

"
En esta ocasión limpio una base de datos que contiene registros sobre criminales. 
Por ejemplo, se conoce su fecha de nacimiento, edad, posición dentro de la banda criminal, 
tipo de crimen, entre otros. El inconveniente es que los datos han sido ingresados con errores. 
Para solucionar aquello, utilizaré la librería Regular Expressions con la cual es posible extraer 
las palabras que necesito.
"


# cargando librerias

library(lubridate) # dmy() funcion para trabajar fechas
library(readxl)
library(stringr)   # libreria para trabajar expresiones regulares 
library(tidyverse) # dplyr, ggplot2, tdyr


# Set Directorio 
user <- Sys.getenv("USERNAME")  # username
setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2022_2/Lab6") ) # set directorio

# Leyendo datos
data <- data.frame(read_excel("../data/crime_data/data_administrativa.xlsx"))
View(data)   # se puede observar que las observaciones tienen errores de ingreso y valores extraños


# Convierto el nombre de las variables a minúscula
colnames(data) <- tolower(colnames(data))   


# Como el nombre de la persona tiene puntuaciones y número, retiro todo aquello 
# que no permita identificar el nombre correcto.
data$solo_nombre <- apply( data['nombre'],
                           1 ,  
                           function(x) gsub("[0-9]|\\/|\\-|\\.|\\!", '', x))  # eliminando números y signos en específico


# Limpio la fecha de nacimiento de aquellos elementos que la ensucian. 
data$fecha <- apply( data['born_date'],
                     1 ,  
                     function(x) str_extract(x,"[0-9]+/[0-9]+/[0-9]+"))


# Luego creo otra variable con el formato de fecha.
data$fecha_formato <-  as.Date(data$fecha, format='%d/%m/%Y')


# Limpio la columna de edad, la cual tiene puntuaciones que no permiten identificarla correctamente.
data$edad <- apply( data['age'],
                    1 ,  
                    function(x) str_extract(x,"[0-9]+"))


# Creo dummies según el rango del sentenciado en la organización criminal
# Ejemplo: dummy1 hace referencia si el criminal es lider de la banda.

data<- data %>% mutate(dummy1 = ifelse( str_detect( rank,"lider de la banda criminal"), 1, 0 ),
                       dummy2 = ifelse( str_detect( rank,"cabecilla local"), 1, 0 ), 
                       dummy3 = ifelse( str_detect( rank,"cabecilla regional"), 1, 0 ),
                       dummy4 = ifelse( str_detect( rank,"sicario"), 1, 0 ),
                       dummy5 = ifelse( str_detect( rank,"extorsion|extorsionador"), 1, 0 ),
                       dummy6 = ifelse( str_detect( rank,"miembro"), 1, 0 ),
                       dummy7 = ifelse( str_detect( rank,"novato|novto|noato|principiante"), 1, 0 ))


# Extraigo el usuario del correo electrónico.
data$usuario_correo <- apply(data['correo_abogado'],
                             1 ,  
                             function(x) str_match(x,"\\w+"))


# Creo una columna que contenga solo la información del número de dni (por ejemplo: 01-75222677)
data$DNI <- apply( data['dni'],
                   1 ,  
                   function(x) str_extract(x,"[0-9]+-[0-9]+"))


# A partir de la columna observaciones, creo las siguientes variables:
# - crimen: contiene información del delito cometido
data$crimen <- sapply(data$observaciones,
                      function(x) str_extract(x,"(?<=sentenciado por )[[a-z]+\\s]*"))

# - n_hijos: cantidad de hijos del criminal
data$n_hijos <- sapply(data$observaciones,
                       function(x) str_extract(x,"(?<=tiene )[0-9]*"))

# - edad_inicio : edad de inicio en actividades criminales
data$edad_inicio <- sapply(data$observaciones,
                           function(x) str_extract(x,"[0-9]+(?= años)"))


# observando la base limpiada
data_limpiada <- data[, c('solo_nombre', 'fecha_formato', 'edad', 'dummy1', 'dummy2', 'dummy3', 
                       'dummy4', 'dummy5', 'dummy6', 'dummy7', 'correo_abogado', 'usuario_correo', 'DNI', 
                       'crimen', 'n_hijos', 'edad_inicio')]

View(data_limpiada)

