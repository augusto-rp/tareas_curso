#Vamos a hacer alguna cuestion con las letras de Lana del Rey

library(tidyverse)
library(stringr)
library(tidytext) #ayuda en la limpieza de datos
library(stm)
library(quanteda)
library(igraph)
library(ggraph)
library(LDAvis)
library(ggplot2)

#genero metadatos 
metadatos<-tribble(
  ~folder, ~nombre_album, ~ano, ~artista, ~discon
  "tarea3_lanadelrey/borntodie","1-B2D", 2012, "lana del rey",1,
  "tarea3_lanadelrey/paradise", "2-Paradise", 2012,"lana del rey",2,
  "tarea3_lanadelrey/ultraviolence", "3-UV", 2014,"lana del rey",3,
  "tarea3_lanadelrey/honeymoon", "4-HM", 2015,"lana del rey",4,
  "tarea3_lanadelrey/lustforlife", "5-L4L", 2017,"lana del rey",5,
  "tarea3_lanadelrey/normanfrockwell", "6-NFR", 2019,"lana del rey",6,
  "tarea3_lanadelrey/chemtrailsovertheclub", "7-COCC", 2021,"lana del rey",7,
  "tarea3_lanadelrey/bluebannisters", "8-BB", 2021,"lana del rey",8,
  "tarea3_lanadelrey/didyouknowthereisatunnel", "9-DYK", 2023,"lana del rey",9,
  "tarea3_lanadelrey/thekickinside", "1-TKI", 1978, "kate bush",1,
  "tarea3_lanadelrey/lionheart", "2-LH", 1978, "kate bush",2,
  "tarea3_lanadelrey/neverforever","3N4E", 1980, "kate bush",3,
  "tarea3_lanadelrey/thedreaming", "4-TD", 1982, "kate bush",4,
  "tarea3_lanadelrey/houndsoflove", "5-HoL", 1985, "kate bush",5,
  "tarea3_lanadelrey/thesensualworld", "6-HoL", 1989, "kate bush",6
)

#leer letras, esto tuve que pedirselo a deepseek
lyrics_list <- list()

for(i in 1:nrow(metadatos)) {
  album_folder <- metadatos$folder[i]
  album_name <- metadatos$nombre_album[i]
  album_year <- metadatos$ano[i]
  artist_name<-metadatos$artista[i]
  album_number<-metadatos$discon[i]
  
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


#vamos a procesar el texto, EN TEORIA todo esto me lo puedo saltar pq stm tiene su propia forma de hacer esto sin agregar columnas
#SIN EMBARGO LO INTENTE CON ESE METODO Y ME PARECIO QUE RECORTABA MUCHO Y ESO AFECTABA RESULTADOS

datos <- datos %>%
  mutate(
    text_clean = str_to_lower(text),
   text_clean = str_replace_all(text_clean, "[^a-zA-Z0-9\\s]", ""),
    text_clean = str_squish(text_clean)
  )


#para eliminar stop words primero tokenizamos

# token<-datos|>
 # unnest_tokens(word, text_clean)|>
#  anti_join(stop_words)




#y agregar una columna
#datos <- token |>
 # group_by(album, year, track_number, track_title, text) |>
  #summarise(text_non = paste(word, collapse = " "), .groups = "drop") |>
  #left_join(select(datos, album, track_title, text_clean), 
#            by = c("album", "track_title"))

#datos <- datos %>%
#  select(album, year, track_number, track_title, text, text_clean, text_non)


#Ahora tengo un objeto con tres versiones de cada letra
#text que es texto original
#text_clean que es texto procesaso y con stop words
#text_non que es procesado y sin stop words

# rm(list=c("lyrics_list","token","metadatos"))


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
  lower.thresh = 3  #solo palabras que aparezcan en al menos tres documentos, lo probe con dos y aparecian algunas palabras aun muy especificas
) 



cat("Original vocabulary size:", length(procesado$vocab), "\n")
cat("Vocabulary after filtering:", length(out$vocab), "\n") #se redujo muchisimo las palabras, que shushsa
cat("Number of documents:", length(out$documents), "\n")

rm(procesado)


################Modelo de muchos topicos, para ver cual balancea mejor exclusividad y coherencia

n_modelos <- data.frame(
  K = c(3, 5, 6, 7, 8, 10, 12),
  exclusivity = NA,
  semantic_coherence = NA
)

for(i in 1:nrow(n_modelos)) {
  k <- n_modelos$K[i]
  model <- stm(
    documents = out$documents,
    vocab = out$vocab,
    K = k,
    max.em.its = 100,
    data = out$meta,
    init.type = "Spectral"
  )
  n_modelos$exclusivity[i] <- mean(exclusivity(model))
  n_modelos$semantic_coherence[i] <- mean(semanticCoherence(model, out$documents))
}

#todos los modelos covergen, pero cual es mejor?

# Cual es mejor?

ggplot(n_modelos, aes(x = semantic_coherence, y = exclusivity, label = K)) +
  geom_point() +
  geom_text(hjust = 1, vjust = 1) +
  labs(title = "Calidad de modelo por numero de topicos")

##esto sugiere el de 5 o el de 3 veamos cada uno por seprado para ver cual hace mas sentido

rm(list=c("lyrics"))


#######modelo de 5

set.seed(3141)
stm_model5 <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = 5,                    # Numero de topicos
  prevalence = ~ album ,  # Topicos varian por album
  max.em.its = 100,          # Maximas iteraciones para lograr convergencia
  data = out$meta,
  init.type = "Spectral"
)

#modelo converge con 5 TEMAS

#EXPLOREMOS
etiquetas <- labelTopics(stm_model5, n = 7)
print(etiquetas)

#frex es frecuencia y exclusividad, palabras que son mas exclusivas de este topico
#implica un 50% de frecuencai y 50% de exclusividad

#lift distingue palabras que tienen mas probabilidad de aparecer en este topico que en otro

#score usa una combinacionde da cuenta de FREFUENCIA Y DISTINVIDIDAD

plot(stm_model5, 
     type = "labels", 
     topics = 1:5,
     labeltype = "frex",  # encuentro que este indicar es el mas interpretable
     n = 7,                # Cuantas palabras mostrar
     main = "Topicos: palabras mas frec y exclusivas")

#esto hace un poco mas interpretable los temas


#veamos como cambian estos temas, primeroe stimamos efectos de album
prep <- estimateEffect(
  1:5 ~ album ,
  stm_model5,
  meta = out$meta,
  uncertainty = "Global"
)


par(mfrow = c(2, 3))
plot(prep, covariate = "album", topics = 1, model = stm_model5, main = "Topic 1 by Album")
plot(prep, covariate = "album", topics = 2, model = stm_model5, main = "Topic 2 by Album")
plot(prep, covariate = "album", topics = 3, model = stm_model5, main = "Topic 3 by Album")
plot(prep, covariate = "album", topics = 4, model = stm_model5, main = "Topic 4 by Album")
plot(prep, covariate = "album", topics = 5, model = stm_model5, main = "Topic 5 by Album")



#Cual es la cancion mas representativa para cada topico?

findThoughts(
  stm_model5,
  texts = out$meta$track_title,  
  topics = 1:5,
  n = 3,
  meta = out$meta
)

#como se relacionan topicos


frex_words <- labelTopics(stm_model5, n = 10, labeltype = "frex")

toLDAvis(stm_model5, out$documents, R = 10) 



#######modelo de 3

set.seed(3141)
stm_model3 <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = 3,                    # Numero de topicos
  prevalence = ~ album ,  # Topicos varian por album
  max.em.its = 100,          # Maximas iteraciones para lograr convergencia
  data = out$meta,
  init.type = "Spectral"
)

#modelo converge con 5 TEMAS

#EXPLOREMOS
etiquetas3 <- labelTopics(stm_model3, n = 7)
print(etiquetas3)

#frex es frecuencia y exclusividad, palabras que son mas exclusivas de este topico
#implica un 50% de frecuencai y 50% de exclusividad

#lift distingue palabras que tienen mas probabilidad de aparecer en este topico que en otro

#score usa una combinacionde da cuenta de FREFUENCIA Y DISTINVIDIDAD

plot(stm_model3, 
     type = "labels", 
     topics = 1:3,
     labeltype = "frex",  # encuentro que este indicar es el mas interpretable
     n = 7,               
     main = "Topicos: palabras mas frec y exclusivas")

#esto hace un poco mas interpretable los temas


#veamos como cambian estos temas, primeroe stimamos efectos de album
prep3 <- estimateEffect(
  1:3 ~ album ,
  stm_model3,
  meta = out$meta,
  uncertainty = "Global"
)


par(mfrow = c(2, 2))
plot(prep3, covariate = "album", topics = 1, model = stm_model3, main = "Topic 1 by Album")
plot(prep3, covariate = "album", topics = 2, model = stm_model3, main = "Topic 2 by Album")
plot(prep3, covariate = "album", topics = 3, model = stm_model3, main = "Topic 3 by Album")


#Cual es la cancion mas representativa para cada topico?

findThoughts(
  stm_model3,
  texts = out$meta$track_title,  
  topics = 1:3,
  n = 3,
  meta = out$meta
)








# Stylometric Analysis ----------------------------------------------------

#como es la voz de lana del rey?





###################otro menos interesante, pero solo para probar
# Concurrencias -----------------------------------------------------------
unique(datos$album)

b2d<-datos|>
  filter(album == "1-B2D")


album_bigrams <- b2d |>   #ngramas de 2, apres de palabras
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)


bigrams_separated <- album_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")



bigram_graph <- bigrams_separated %>%
  filter(n > 5) %>% # solo conexiones que aparezcan mas de 5 veces, sino era ilegible
  graph_from_data_frame()

# Plot the network
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()