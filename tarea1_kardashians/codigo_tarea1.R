#Vamos a analizar las emociones presentes en el capitulo final de la primera temporada de The Kardashians
#Para ello necesitamos abrir el siguiente paquete
 library(text2emotion)
 library(readr) #para leer txt
texto<-read_file("tarea1_kardashians/1_10.txt")

#Sin embargo, el texto esta lleno de simbolos que hay que eliminar. Por lo 
#vamos a usar ademas una funcion inserta en text2emotion que va a expandir cotnradcciones, y estandarizar puntuacion
#texto<-preprocess_text(texto)
#texto



#Al comparar el primer archivo con el segundo.Pasa algo raro "be able to expand\r\nour family one day" pasa a "be able to e tongue sticking out and our family one day"
#No tengo idea que paso   "e tongue sticking out". Talvez aso paso cin \r and \n
#Entonces voy a volver a crear texto sin ningun cambio y primero sacarle esos signos -->Volver a linea 5 y luego aplicar 16
texto<-gsub("\r","" , texto)
texto

#Al volver a aplicar linea 9 vuelve a haber ese mismo problema. Posiblemente algun proceso de la funcion esta traduciendo algo en un emoji. Por lo tanto no se usara esa funcion
#Por lo tanto orden es linea 5-->linea16


#Debo agregar lineas al texto. Despues de cada "." una nueva linea

texto <- unlist(strsplit(texto, "\n"))
texto

#eliminar espacios vacios
texto <- gsub("^\\s+|\\s+$", "", texto)
texto <- texto[nchar(texto) > 0]

#Hasta ahora todo bien, ahora bien, como se ve el texto indica hablante "KRIS:" "TRAVIS:", etc
#Aca hay dsoposibilidades usar esa como una variable de hablante. Que implicaria convertir text en una tabla donde fila indique hablante y lo que dice.
#O simplemente eliminar esa parte y quedarnos con el texto puro

#Primera opcion-->Separar lineas por habalantes
#Crear una tabla con dos columnas hablante y texto
#library(dplyr)
#library(stringr)

#texto_linea<-read_file("tarea1_kardashians/1_10.txt") 
#texto_linea<-gsub("\r\n","" , texto_linea)
#texto_linea #objeto donde todo es uan linea

#texto_separado <- strsplit(texto_linea, "(?<=[.!?\\-])(?=[A-Z]{2,}:)|(?<=[^A-Z!?.\\-])(?=[A-Z]{2,}:)", perl = TRUE)[[1]]
#texto_separado #separar por hablantes. pero parece que la transcripcion no es suficientemente consistente para permitir esto
rm(patron)
#Quiero eliminar todos los "" en texto
texto <- texto[texto != ""]

#Lexicons emocionales
library(tidytext)
library(textdata)

afinn <- get_sentiments("afinn") #valor entre -5 y +5 de valencia emocional de palabras
bin <- get_sentiments("bing") #palabras positivas y negativas categorizacion binaria
nrc <- get_sentiments("nrc") #varias emociones: alegria, tristeza, ira, miedo, sorpresa, asco, confianza, anticipacion


#Ahora bien hay que ser cuidadoso al usar estas bases pues no manejan bien dobles negativos, sarcamos y otros aspectos pragmaticos del lenguaje
#Por ejemplo "not happy" seria clasificado como feliz por la presencia de happy

#ojo, objeto no puede ser character debe ser un objeto tipo tibble o data frame
#debo convertir texto en data frame
texto_df <- data.frame(text = texto, stringsAsFactors = FALSE)

#El proceso siguiente es tokenizar el texto y asociarlo a dichas emociones
texto_afinn<-texto_df|>
  unnest_tokens(word, text) |>
  inner_join(afinn, by = "word")



texto_bin<-texto_df|>
  unnest_tokens(word, text) |>
  inner_join(bin, by = "word")


texto_nrc<-texto_df|>
  unnest_tokens(word, text) |>
  inner_join(nrc, by = "word")
