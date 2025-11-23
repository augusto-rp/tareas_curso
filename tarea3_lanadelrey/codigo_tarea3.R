#Vamos a hacer alguna cuestion con las letras de Lana del Rey

library(tidyverse)
library(stringr)
library(tidytext) #ayuda en la limpieza de datos
library(stm)
library(quanteda)
library(igraph)
library(LDAvis)

#genero metadatos 
metadatos<-tribble(
  ~folder, ~nombre_album, ~ano,
  "tarea3_lanadelrey/borntodie","1-B2D", 2012,
  "tarea3_lanadelrey/paradise", "2-Paradise", 2012,
  "tarea3_lanadelrey/ultraviolence", "3-UV", 2014,
  "tarea3_lanadelrey/honeymoon", "4-HM", 2015,
  "tarea3_lanadelrey/lustforlife", "5-L4L", 2017,
  "tarea3_lanadelrey/normanfrockwell", "6-NFR", 2019,
  "tarea3_lanadelrey/chemtrailsovertheclub", "7-COCC", 2021,
  "tarea3_lanadelrey/bluebannisters", "8-BB", 2021,
  "tarea3_lanadelrey/didyouknowthereisatunnel", "9-DYK", 2023
  
)

#leer letras, esto tuve que pedirselo a deepseek
lyrics_list <- list()

for(i in 1:nrow(metadatos)) {
  album_folder <- metadatos$folder[i]
  album_name <- metadatos$nombre_album[i]
  album_year <- metadatos$ano[i]
  
  cat("Processing:", album_name, "\n")
  
  # Get all text files
  txt_files <- list.files(album_folder, pattern = "\\.txt$", full.names = TRUE)
  
  # Read each file
  for(file in txt_files) {
    lyrics_text <- readLines(file, warn = FALSE) %>% paste(collapse = " ")
    track_name <- tools::file_path_sans_ext(basename(file))
    
    lyrics_list[[length(lyrics_list) + 1]] <- data.frame(
      album = album_name,
      year = album_year,
      track_title = track_name,
      text = lyrics_text
    )
  }
}

# Crear data frame
datos <- bind_rows(lyrics_list) %>%
  group_by(album) %>%
  mutate(track_number = row_number()) %>%
  ungroup() %>%
  select(album, year, track_number, track_title, text)


#vamos a procesar el texto

datos <- datos %>%
  mutate(
    text_clean = str_to_lower(text),
    text_clean = str_replace_all(text_clean, "[^a-zA-Z0-9\\s]", ""),
    text_clean = str_squish(text_clean)
  )


#para eliminar stop words primero tokenizamos

token<-datos|>
  unnest_tokens(word, text_clean)|>
  anti_join(stop_words)




#y agregar una columna
datos <- token |>
  group_by(album, year, track_number, track_title, text) |>
  summarise(text_non = paste(word, collapse = " "), .groups = "drop") |>
  left_join(select(datos, album, track_title, text_clean), 
            by = c("album", "track_title"))

datos <- datos %>%
  select(album, year, track_number, track_title, text, text_clean, text_non)


#Ahora tengo un objeto con tres versiones de cada letra
#text que es texto original
#text_clean que es texto procesaso y con stop words
#text_non que es procesado y sin stop words

rm(token)


# Structural Topic Modelling ----------------------------------------------

#un upgrade respecto a LDA que fue lo que se hizo en tarea anterios

#CREACION DE CORPUS CON DATOS QUE INCLUYEN STOP WORDS
procesado <- textProcessor(
  documents = datos$text_clean,
  metadata = datos,
  lowercase = FALSE,  #LOL, PODRIA HABERME SALTADO LO ANTERIOR
  removestopwords = FALSE,  
  removenumbers = FALSE,    
  removepunctuation = FALSE, 
  stem = FALSE,              # NO quiero ahcer stemming que no funciona tan bien 
  wordLengths = c(3, Inf)   
)

#PREPARACION PARA STM
out <- prepDocuments(
  documents = procesado$documents,
  vocab = procesado$vocab,
  meta = procesado$meta,
  lower.thresh = 2  #solo palabras que aparezcan en al menos dos documentos
)


cat("Original vocabulary size:", length(procesado$vocab), "\n")
cat("Vocabulary after filtering:", length(out$vocab), "\n") #se redujo muchisimo las palabras, que shushsa
cat("Number of documents:", length(out$documents), "\n")

rm(procesado)

#modelo

set.seed(3141)
stm_model <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = 5,                    # Number of topics
  prevalence = ~ album + s(track_number),  # Topics vary by album and track position
  max.em.its = 100,          # Maximum iterations
  data = out$meta,
  init.type = "Spectral"
)

#modelo converge con 5 TEMAS

#EXPLOREMOS
etiquetas <- labelTopics(stm_model, n = 7)
print(etiquetas)

#frex es frecuencia y exclusividad, palabras que son mas exclusivas de este topico
#implica un 50% de frecuencai y 50% de exclusividad

#lift distingue palabras que tienen mas probabilidad de aparecer en este topico que en otro

#score usa una combinacionde da cuenta de FREFUENCIA Y DISTINVIDIDAD

plot(stm_model, 
     type = "labels", 
     topics = 1:5,
     labeltype = "frex",  # encuentro que este indicar es el mas interpretable
     n = 7,                # Number of words to show per topic
     main = "Topics with Highest Score Words")

#esto hace un poco mas interpretable los temas


#veamos como cambian estos temas, primeroe stimamos efectos de album
prep <- estimateEffect(
  1:5 ~ album ,
  stm_model,
  meta = out$meta,
  uncertainty = "Global"
)


par(mfrow = c(2, 3))
plot(prep, covariate = "album", topics = 1, model = stm_model, main = "Topic 1 by Album")
plot(prep, covariate = "album", topics = 2, model = stm_model, main = "Topic 2 by Album")
plot(prep, covariate = "album", topics = 3, model = stm_model, main = "Topic 3 by Album")
plot(prep, covariate = "album", topics = 4, model = stm_model, main = "Topic 4 by Album")
plot(prep, covariate = "album", topics = 5, model = stm_model, main = "Topic 5 by Album")


#Cual es la cancion mas representativa para cada topico?

findThoughts(
  stm_model,
  texts = out$meta$track_title,  
  topics = 1:5,
  n = 3,
  meta = out$meta
)

#como se relacionan topicos

par(mfrow=c(1,1))
topic_corr <- topicCorr(stm_model)
plot(topic_corr)



frex_words <- labelTopics(stm_model, n = 10, labeltype = "frex")

toLDAvis(stm_model, out$documents, R = 10) 
