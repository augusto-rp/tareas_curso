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
library(dplyr)
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


texto_afinn|>
  summarise(overall_score= sum(value, na.rm=TRUE)) #es la suma, si tengo 2477 y cada valor puede girar entre -5 y +5 
                                                  #entonces si todas las palarbas fueran lo maximo positivo posible el valor seria 12.385
  
texto_bin|>
  count(sentiment, sort=TRUE)


texto_nrc|>
  count(sentiment, sort=TRUE)


#Quiero hacer una tabla descriptiva con las 10 palabras mas comunes positivas y negativas usando texto_bin
library(dplyr)
top_palabras_bin <- texto_bin %>%
  group_by(sentiment, word) %>%
  summarise(frecuencia = n()) %>%
  arrange(sentiment, desc(frecuencia)) %>%
  slice_head(n = 10)
top_palabras_bin

#Ahora quiero hacer una tabla descriptiva con las 3 palabras mas comunes por cada emocion usando texto_nrc
top_palabras_nrc <- texto_nrc %>%
  group_by(sentiment, word) %>%
  summarise(frecuencia = n()) %>%
  arrange(sentiment, desc(frecuencia)) %>%
  slice_head(n = 3)
top_palabras_nrc

library(ggplot2)
#Finalmente quiero graficar los resultados de top_palabras_nrc
ggplot(top_palabras_nrc, aes(x = reorder(word, -frecuencia), y = frecuencia, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(title = "Top 3 palabras más comunes por emoción",
       x = "Palabras",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##no me gusta esta tabla, 
#se me ocurrio idea de ver palabras que aparezcan en multiples categorias mas de 10 veces
palabras_multiples <- texto_nrc %>%
  group_by(word, sentiment) %>%
  summarise(frecuencia = n()) %>%
  ungroup() %>%
  group_by(word) %>%
  filter(n() > 1 & frecuencia > 10) %>%
  arrange(word, desc(frecuencia))
palabras_multiples

#Crear un grafico de barras donde aparezcan las 4 palabras mas usadas para expresar sentimientos negativos y las 4 mas comunes para expresar sentimientos positivos usando  top_palabras_bin
top_palabras_bin_filtered <- top_palabras_bin |>
  filter(sentiment %in% c("positive", "negative")) |>
  group_by(sentiment) |>
  slice_head(n = 4) |>
  ungroup()
#ahora crear el grafico
ggplot(top_palabras_bin_filtered, aes(x = reorder(word, -frecuencia), y = frecuencia, fill = sentiment)) +
  geom_bar(stat = "identity") +Grafi
  labs(title = "Top 4 palabras más comunes para sentimientos positivos y negativos",
       x = "Palabras",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Cuidado todos estos resultados son engañosos. Pues la mayoria de god son en contextos de "oh my god" lo que hacen los programas es 
#decir todas las amociones posibles asociadas a ello. Es decir, emociones a las que se asocia  "god", esto no es lo mismo a decir que uso se les da