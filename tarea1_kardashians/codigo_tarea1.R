#Vamos a analizar las emociones presentes en el capitulo final de la primera temporada de The Kardashians
#Para ello necesitamos abrir el siguiente paquete
 library(text2emotion)
 library(readr) #para leer txt
texto<-read_file("tarea1_kardashians/1_10.txt")

#Sin embargo, el texto esta lleno de simbolos que hay que eliminar. Por lo tanto vamos a arreglarlo un poco
  