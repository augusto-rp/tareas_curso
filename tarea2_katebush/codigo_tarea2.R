
library(quanteda)
library(topicmodels)
library(readtext)


corp<-corpus_reshape(data_corpus_inaugural, to="paragraphs")
                                      #data_corpus_inaugural que viene con paquete
                                      #es pasado a parrafos

#dfm es un objeto del paquete de quanteda que crea matriz de terminos

dfm<- corp |>
  tokens(remove_punct = TRUE)|>
  tokens_remove(stopwords("en")) 
#en la nueva version de quanteda hay que tokenizar uno el material antes de pasarlo a dfm
#corpus->tokens-->DFM

dfm<-dfm(dfm) #crear el dfm


dfm<-dfm_trim(dfm,min_docfreq=5) #solo palabras que aparezcan 5 veces

#Topic Modelling es bien demandante computacionalmente asi que este paso es importante cuando son bases grandes


#el paquete de topicmodels requiere convertir a otro formato
dtm<-convert(dfm,to="topicmodels")

set.seed(1) #replicabilidad
m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
#LDA es probabilistico, por eso importante el seed
#metodo gibbs 
#k es el numero de temas. Idealmente se aprende con los datos
#Control especifica parametros de distribucion de temas. Entre mas alto alfa mas mixtura de topicos


#Veamos los terminos que componen temas
terms(m, 5) #el orden indica importancia


#Podemos sabeer la importancia de cada palabra por topico

topic=3
  words=posterior(m)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
  topwords =head(sort(words, decreasing = T), n=50) #ordenar palabras por relevancia
  head(topwords)
#estos valores lo que me dicen es que tan relevante es cada palabra dentro del topico
  #INDAGAR MAS
  
  library(wordcloud)  
  wordcloud(names(topwords), topwords)
  

# Datos Kate Bush- The Dreaming -------------------------------------------
  letras_canciones<- list.files("tarea2_katebush/letras", pattern = "\\.txt$", full.names = TRUE)
  canciones<-corpus(readtext(letras_canciones)) #leer todos los txt en un corpus

  dfm_kb<- canciones |>
    tokens(remove_punct = TRUE)|>
    tokens_remove(stopwords("en")) 
  dfm_kb<-dfm(dfm_kb) #crear el dfm 

  # y ahora convertir a dtm  
  
  dtm_kb<-convert(dfm_kb,to="topicmodels")
  
  set.seed(1) #replicabilidad
  m_kb = LDA(dtm_kb, method = "Gibbs", k = 10,  control = list(alpha = 0.1)) #10 temas, 10 canciones
  
  terms(m_kb, 5)
#me llama atencion topico


  inspect(dtm_kb[1:10,1:10]) #ver las primeras 10 canciones y las palabras 1 a 10
  
  
  topic=2
  words_kb=posterior(m_kb)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
  topwords_kb =head(sort(words_kb, decreasing = T), n=50) #ordenar palabras por relevancia
  head(topwords_kb)
  #probabilidad de cada palabra este asociada a topico 2
  
  
  wordcloud(names(topwords_kb), topwords_kb)
#mas asociado a pull out the pin  
  #BUT FRIENDS DONT LET FRIENDS MAKE WORDCLOUDS
  #CREA UN HISTOGRAMA DE FRECUENCIA DE LAS 10 PALABRAS MAS IMPORTANTES EN TOPICO 2
  top10_kb<- head(sort(words_kb, decreasing = T), n=10)
  barplot(top10_kb, las=2, col="lightblue", main="Top 10 palabras del tópico 2 en canciones de Kate Bush", ylab="Importancia")  

  
  #### ES RECOMENDABLE QUE SE EXPLORE OTRA CANTIDAD K DE TOPICOS
  
  
  
  

# OTROS DATOS -GUIA DE NEOREACCIONARIOS -----------------------------------

install.packages("epubr")  
library(epubr)  

  epub_data <- epub("tarea2_katebush/otros_textos/neoreaccionario.epub")
  
  
  # Extraer texto del epuc
  text_content <- epub_data$data[[1]]$text
  
  # Combinar todo en un vector
  full_text <- paste(text_content, collapse = "\n\n")
  #tengo que hacer preprocesamiento de este texto
  
  
  #usar raiz "democra", no considerar chapter, "govern", "polit"
  
  # Crear archivo text, OJO que aparece en carpeta princopal
  writeLines(full_text, "output_file.txt")
  
  
  

  #Convertirlo en corpus
  neor<-readLines("tarea2_katebush/otros_textos/output_file.txt")
  neor_corp<-corpus(neor)
  
  
  
  rm(list=c("neor", "full_text","epub_data"))
  
  
  dfm_neo<- neor_corp |>
    tokens(remove_punct = TRUE)|>
    tokens_remove(stopwords("en")) 
  
  
  dfm_neo<-dfm(dfm_neo) #crear el dfm 
  dfm_neo<-dfm_trim(dfm_neo,min_docfreq=5) #solo palabras que aparezcan 10 veces 
  dtm_neo<-convert(dfm_neo,to="topicmodels")
rm(dfm_neo)


m_neo = LDA(dtm_neo, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
terms(m_neo, 10)


#hacer antijoin para eliminar palabra chapter
library(dplyr)
topic=1

