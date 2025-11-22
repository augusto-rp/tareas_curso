#Vamos a hacer alguna cuestion con las letras de Lana del Rey

library(tidyverse)
library(stringr)

#genero metadatos 
metadatos<-tribble(
  ~folder, ~nombre_album, ~ano,
  "tarea3_lanadelrey/borntodie","born to die", 2012,
  "tarea3_lanadelrey/paradise", "paradise", 2012,
  "tarea3_lanadelrey/ultraviolence", "ultraviolence", 2014,
  "tarea3_lanadelrey/honeymoon", "honeymoon", 2015,
  "tarea3_lanadelrey/lustforlife", "lust for life", 2017,
  "tarea3_lanadelrey/normanfrockwell", "norman fucking rockwell", 2019
)

#leer letras
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

datos <- bind_rows(lyrics_list) %>%
  group_by(album) %>%
  mutate(track_number = row_number()) %>%
  ungroup() %>%
  select(album, year, track_number, track_title, text)