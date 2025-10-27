
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
  
  
  
  topic=2
  words_kb=posterior(m_kb)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
  topwords_kb =head(sort(words_kb, decreasing = T), n=50) #ordenar palabras por relevancia
  head(topwords_kb)
  
  
  wordcloud(names(topwords_kb), topwords_kb)
#mas asociado a pull out the pin  
  