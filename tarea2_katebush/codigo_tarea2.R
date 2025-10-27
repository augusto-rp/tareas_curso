
library(quanteda)
library(topicmodels)
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
  