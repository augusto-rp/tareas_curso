
library(tm) #funciones de pre procesamiento de texto
library(epubr) #abrir epub
library(dplyr) #operaciones de pre procesamietno de texto
library(quanteda) #crear dfm
library(topicmodels)#analisis
library(readtext) #abrir y crear archivo txt
library(textclean) #transformar contracciones
library(SnowballC) #para transformar palabas en sus raices (stemming)
library(tidyverse) #graficos




#TOPIC ANALYSIS USIND LDA GUIA DE NEOREACCIONARIOS -----------------------------------



epub_data <- epub("tarea2_katebush/otros_textos/neoreaccionario.epub")


# Extraer texto del epuc
text_content <- epub_data$data[[1]]$text

# Combinar todo en un vector
full_text <- paste(text_content, collapse = "\n\n")


#tengo que hacer preprocesamiento de este texto


#poner todo en minuscula
full_text <- tolower(full_text)

#eliminar saltos de linea
gsub("\n", "", full_text)

#eliminar espacios donde haya multiples espacios
full_text <- gsub("\\s+", " ", full_text)

#EXPANDIR CONTRACCIONES EN INGLES
full_text <- replace_contraction(full_text)

grepl("aren't", full_text) #para verificar

#eliminar puntuacion en full_text
full_text <- gsub("[[:punct:]]", " ", full_text)

#eliminar stopwords de full_text
full_text <- removeWords(full_text, stopwords("english"))
full_text

#tokenizar full_text
full_text<- unlist(strsplit(full_text, " "))


#hacer stemming de full_text

full_text_stem <- wordStem(unlist(strsplit(full_text, " ")), language = "english")
full_text_stem



# Crear archivo text, OJO que aparece en carpeta princopal HACERLO SOLO UNA VEZ
#writeLines(full_text_stem, "tarea2_katebush/otros_textos/output_file_stem.txt")

#writeLines(full_text, "output_file.txt") #version sin stemming





#Convertirlo en corpus
neor <- readLines("tarea2_katebush/otros_textos/output_file_stem.txt")
neo_c<- corpus(neor)

#si quiero hacer lo mismo con stem solo cambiar nombre de archivo en linea anterior

neo_q<-as.corpus(neo_c)
neo_tk<-tokens(neo_c) #

dfm_neo<-dfm(neo_tk)

rm(list=c("dfm_neo","neo_tk","neo_c", "neor"))

#y ahora psar de dfm a dtm
dtm_neo <- convert(dfm_neo, to = "topicmodels")
rm(dfm_neo)


set.seed(3141)
m_neo = LDA(dtm_neo,
            method = "Gibbs",
            k = 8,
            control = list(alpha = 0.5))  #ajuste el alpha a 0.5 que permite mayor solapamiento de palabras entre distintos topicps
terms(m_neo, 8)

#Hoppe hace referencia a Hans-Hermann Hope 
#indo hace referencia a indo-europeans

set.seed(3141)
m_neo5 = LDA(dtm_neo,
            method = "Gibbs",
            k = 5,
            control = list(alpha = 0.5))
terms(m_neo5, 10)


set.seed(3141)
m_neo6 = LDA(dtm_neo,
             method = "Gibbs",
             k = 6,
             control = list(alpha = 0.5))
terms(m_neo6, 10)

set.seed(3141)
m_neo4 = LDA(dtm_neo,
             method = "Gibbs",
             k = 4,
             control = list(alpha = 0.5))
terms(m_neo4, 10)


#Despues de repetidas corridas me parece que es mejor usar la version con stemming
#ADEMAS k ENTRE 5 Y 8. Para simplificar sera 5


topic = 2
words_kb = posterior(m_neo5)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
topwords_kb = head(sort(words_kb, decreasing = T), n = 50) #ordenar palabras por relevancia
head(topwords_kb)
#probabilidad de cada palabra este asociada a topico 2

#ojo con letras sueltas, contracciones posiblemente habria que volver a procesamiento inicial

#Hacer grafico con las 5 palabras ams comunes en los 5 topicos de m_neo5

#Metodo R base
par(mfrow=c(3,2)) #configurar grafico 3 filas 2 columnas
for (i in 1:5) {
  topic = i
  words_kb = posterior(m_neo5)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
  top5_kb = head(sort(words_kb, decreasing = T), n = 5) #ordenar palabras por relevancia
  barplot(
    top5_kb,
    las = 2,
    col = "lightblue",
    main = paste("Top 10 palabras del tópico", i, " en A Critique of Democracy: A Guide for NeoReactionaries"),
    ylab = "Importancia"
  )
}

#Importancai indicaon relativa dentro del topico no frecuencia absoluta


#Metodo ggplot

#Me gustaria que los 5 graficos aparezcan en un solo grafico
datos<-list()
for (i in 1:5){
  words_kb = posterior(m_neo5)$terms[i, ] #distribucion posterior de terminos para topic definido antes
  top6_kb = head(sort(words_kb, decreasing=T), n=6)
  temp_df<-data.frame(
    Word= names(top6_kb),
    Importance = top6_kb,
    Topic= factor(i)
  )
  datos[[i]]<-temp_df #importante el [[i]] para guardar lsita de tablas en datos
}

datos_grafico <- bind_rows(datos)

#y ahora grafico
ggplot(datos_grafico, aes(x = Importance, y = reorder(Word, Importance), fill = Topic)) +
  
 #Crear barra horizonta
  geom_col() +
  
  # Separar graficso por topico
  # The 'scales = "free_y"' ensures that the word labels for each topic are shown cleanly
  facet_wrap(~ Topic, scales = "free_y") +
  
  # Agregar tituclos
  labs(
    title = "Top 5 palabras más importantes en A Critique of Democracy",
    y = "Palabra",
    x = "Importancia"
  ) +
  
  # Tema minimalista
  theme_minimal() +
  
  # Optional: Customize the title of each small plot (strip)
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5)
  )


# ANALISIS PREVIOS-NO CONSIDERAR ------------------------------------------


corp <- corpus_reshape(data_corpus_inaugural, to = "paragraphs")
#data_corpus_inaugural que viene con paquete
#es pasado a parrafos

#dfm es un objeto del paquete de quanteda que crea matriz de terminos

dfm <- corp |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("en"))
#en la nueva version de quanteda hay que tokenizar uno el material antes de pasarlo a dfm
#corpus->tokens-->DFM

dfm <- dfm(dfm) #crear el dfm


dfm <- dfm_trim(dfm, min_docfreq = 5) #solo palabras que aparezcan 5 veces

#Topic Modelling es bien demandante computacionalmente asi que este paso es importante cuando son bases grandes


#el paquete de topicmodels requiere convertir a otro formato
dtm <- convert(dfm, to = "topicmodels")

set.seed(1) #replicabilidad
m = LDA(dtm,
        method = "Gibbs",
        k = 10,
        control = list(alpha = 0.1))
#LDA es probabilistico, por eso importante el seed
#metodo gibbs
#k es el numero de temas. Idealmente se aprende con los datos
#Control especifica parametros de distribucion de temas. Entre mas alto alfa mas mixtura de topicos
#alfa igual a uno implica una distribucion uniform -misma posibilidad de pertener a cualquier grupo, menor a 1 que cada palabra pertenece a un solo topico y mayor a uno implica que cada palabra pertenece a todos los topicos
#alga es uno de los componentes de la distribucion Dirichlet


#Veamos los terminos que componen temas
terms(m, 5) #el orden indica importancia


#Podemos sabeer la importancia de cada palabra por topico

topic = 3
words = posterior(m)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
topwords = head(sort(words, decreasing = T), n = 50) #ordenar palabras por relevancia
head(topwords)
#estos valores lo que me dicen es que tan relevante es cada palabra dentro del topico
#INDAGAR MAS

library(wordcloud)
wordcloud(names(topwords), topwords)


# Datos Kate Bush- The Dreaming -------------------------------------------
letras_canciones <- list.files("tarea2_katebush/letras",
                               pattern = "\\.txt$",
                               full.names = TRUE)
canciones <- corpus(readtext(letras_canciones)) #leer todos los txt en un corpus

dfm_kb <- canciones |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("en"))
dfm_kb <- dfm(dfm_kb) #crear el dfm

# y ahora convertir a dtm

dtm_kb <- convert(dfm_kb, to = "topicmodels")

set.seed(1) #replicabilidad
m_kb = LDA(dtm_kb,
           method = "Gibbs",
           k = 10,
           control = list(alpha = 0.1)) #10 temas, 10 canciones

terms(m_kb, 5)
#me llama atencion topico


inspect(dtm_kb[1:10, 1:10]) #ver las primeras 10 canciones y las palabras 1 a 10


topic = 2
words_kb = posterior(m_kb)$terms[topic, ] #distribucion posterior de terminos para topic definido antes
topwords_kb = head(sort(words_kb, decreasing = T), n = 50) #ordenar palabras por relevancia
head(topwords_kb)
#probabilidad de cada palabra este asociada a topico 2


wordcloud(names(topwords_kb), topwords_kb)
#mas asociado a pull out the pin
#BUT FRIENDS DONT LET FRIENDS MAKE WORDCLOUDS
#CREA UN HISTOGRAMA DE FRECUENCIA DE LAS 10 PALABRAS MAS IMPORTANTES EN TOPICO 2
top10_kb <- head(sort(words_kb, decreasing = T), n = 10)
barplot(
  top10_kb,
  las = 2,
  col = "lightblue",
  main = "Top 10 palabras del tópico 2 en canciones de Kate Bush",
  ylab = "Importancia"
)


#### ES RECOMENDABLE QUE SE EXPLORE OTRA CANTIDAD K DE TOPICOS


