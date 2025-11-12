<details>
<summary>TAREA 2</summary>

# **Tarea 2**

---

El objetivo de esta tarea es realizar un análisis temática del
libro: “A Critique of Democracy: a Guide for Neoreactionaries”( 2015) de M. Anissimov.
Para ello se usará Latent Dirichlet
Allocation (LDA). 
Si bien este método de análisis temático ha [perdido
relevancia a raíz de desarrollos en inteligencia artificial](https://towardsdatascience.com/is-lda-topic-modeling-dead-9543c18488fa/)
y de algunas limitaciones en su metodología (la principal, que no estima el
número de tópicos a evaluar sino que este número debe ser indicado por el
investigador) igual se usará con el fin de evaluar su utilidad práctica en el
análisis de textos relativamente pequeños.
Para el análisis se requieren las
siguientes librerías:

<div style="font-size: small; line-height: 0.9;">
`library(tm)` #funciones de pre procesamiento de texto
<br>`library(epubr)` #abrir epub
<br>`library(dplyr)` #operaciones de pre procesamiento de texto
<br>`library(quanteda)` #crear dfm
<br>`library(topicmodels)` #analisis
<br>`library(readtext)` #abrir y crear archivo txt
<br>`library(textclean)` #transformar contracciones
<br>`library(SnowballC)` #para transformar palabas en sus raices (stemming)
<br>`library(tidyverse)` #graficos
</div>

---

## 1. **Ordenar**

El archivo que vamos a utilizar esta en extensión epub y se encuentra [aquí](https://github.com/augusto-rp/tareas_curso/blob/master/tarea2_katebush/otros_textos/neoreaccionario.epub)

### Lo primero entonces es abrirlo

```r
epub_data <- epub("tarea2_katebush/otros_textos/neoreaccionario.epub")
#Extraemos el texto y generamos un solo vector

text_content <- epub_data$data[[1]]$text

full_text <- paste(text_content, collapse = "\n\n")
```

### Debemos hacer cierto procesamiento de datos 

```r
#poner todo en minuscula
full_text <- tolower(full_text)

#eliminar saltos de linea
gsub("\n", "", full_text)

#eliminar espacios donde haya multiples espacios
full_text <- gsub("\\s+", " ", full_text)

#Expandir contracciones en ingles
full_text <- replace_contraction(full_text)

#Eliminar puntuacion en full_text
full_text <- gsub("[[:punct:]]", " ", full_text)

#Eliminar stopwords de full_text. Estas son palabras que suelen de carecer contenido semantico en si mismo
full_text <- removeWords(full_text, stopwords("english"))
full_text
```
Ahora vamos a tokenizar el texto, es decir separarlo en "fichas" de palabras.
Esto permitira posteriormente acortar las palabras a su "raiz"

```r
#tokenizar full_text
full_text<- unlist(strsplit(full_text, " "))

#hacer stemming de full_text
full_text_stem <- wordStem(unlist(strsplit(full_text, " ")), language = "english")

#Para observar como queda el texto
full_text_stem
```

Y ahora crearemos un archivo txt con este outcome pues posteriormente convertiremos en coprus para poder realizar analisis

```r
#Archivo con stemming
writeLines(full_text_stem, "tarea2_katebush/otros_textos/output_file_stem.txt")
```


## 2. **Transformación de datos**

El primer paso sera convertir el archivo txt en corpus pues este formateo es necesario para usar librerias de LDA

```r
#Convertirlo en corpus 👁️ que estamos usando archivo creado en paso anterior 👁️
neor <- readLines("tarea2_katebush/otros_textos/output_file_stem.txt")
neo_c<- corpus(neor)

#Retokenizar archivo ahora en formato corpus
neo_tk<-tokens(neo_c)

#Esto lo convierte en un tipo de data frame (DFM) pero es solo un paso previo para convertirlo en el tipo de dataframe que usa topicmodels (DTM)
dfm_neo<-dfm(neo_tk)

#Y ahora si al formato DTM que es el que vamos a usar
dtm_neo <- convert(dfm_neo, to = "topicmodels")
```

Como veran hemos creado muchos objetos innecesarios en el camino, asi que vamos a borrarlos

```r
rm(list=c("dfm_neo","neo_tk","neo_c", "neor"))
```

Y ahora si podemos pasar a realizar los análisis


## 3. **Modelar/Visualizar**
 Un primer paso es que tenemos que indicarle al codigo cuantos tópicos buscar, este es uno de las limitaciones de este tipo de análisis que ha llevado a priorizar otro tipo de análisis basados en IA.
 Pero no estoy en condiciones de aprender a programar en Phyton y generar un LLM en una semana
 
 Entonces primero haremos un modelo con 8 tópicos para ver como nos va
 
```r
 Siempre poner semilla!!!
set.seed(3141)
m_neo = LDA(dtm_neo,
            method = "Gibbs",
            k = 8,
            control = list(alpha = 0.5))  #ajuste el alpha a 0.5 que permite mayor solapamiento de palabras entre distintos topicos
            
#Solicitamos que nos diga las 8 palabras mas comunes
terms(m_neo, 8)
```

 Una observacion sobre **alpha** : entre más alejado de 1 más se supone que las "pertenence" a un solo tópico. Y entre mas alejado de uno, mas se solapan entre si
 
 Vemos algunas cosas raras, como que en el topic 3 "s" es una palabra. Me entra la duda de si sera resultado de tokenizar el posesivo 
 
```r
set.seed(3141)
m_neo5 = LDA(dtm_neo,
            method = "Gibbs",
            k = 5,
            control = list(alpha = 0.5))
terms(m_neo5, 8)
```
 Aca ya empieza a ser un poco mas facil interpretar
 El topico 1 parece hablar de crecimiendo economico, el topico dos parece hablar de una mezcla de temas economicos y electorales, el 3 no queda tan claro
 El topico 4 menciona "hopp" que hace referencia a  a Hans-Hermann Hope un filosofo paleolibertario y anarcocapitalista. Por lo que podeos suponer que tiene que ver con argumentos de este autor
 El topico 5 parece hablar de aspectos sociales y culturales de la democracia
 
 Haremos un nuevo intento con 4 topicos
 
```r
set.seed(3141)
m_neo4 = LDA(dtm_neo,
             method = "Gibbs",
             k = 4,
             control = list(alpha = 0.5))
terms(m_neo4, 8)
```


## 4. **Comunicar**
 
![Se presentan 5 gráficos de barras. Cada uno representa las
6 palabras con mayor relevancia para cada uno de los 5 topicos](https://github.com/augusto-rp/tareas_curso/blob/master/grafico_tarea2.jpeg)

</details>







<details>

  
<summary>TAREA 1</summary>

# **Tarea 1**

El objetivo de esta primera tarea es realizar un análisis de las emociones presentes en un capítulo del programa "The Kardashians". Se utilizará la transcripción del décimo capítulo de la primera temporada disponible en [el siguiente link](https://transcripts.foreverdreaming.org/viewforum.php?f=2354#google_vignette). Para un resumen del capítulo [pinchar aquí](https://en.wikipedia.org/wiki/The_Kardashians#Season_1_(2022)).

Para ello se usará la librería tidytext que cuenta con distintos lexicones emocionales que permiten asignarles distintos valores a las palabras de acuerdo a diversos criterios:

• Afinn: Asigna un valor entre -5 y +5 a las palabras de acuerdo a su valencia emocional. Siendo -5 extremadamente negativa y +5 extremadamente positiva

• Bing: Binariza las palabras entre valencia positiva y negativa

• NRC: Categoriza palabras en función a categoría emocional de pertenencia.

## **1. Ordenamiento de datos**

Al descargar la transcripción esta corresponde a un solo hilo de texto en formato txt. Hay que procesar el texto eliminando símbolos. Inicialmente se usó la función preprocess_text de la librería "text2emotion", sin embargo, esta modificó el texto en algunas partes. Por ejemplo: "be able to expand`\r\n`our family one day" fue transformado a "be able to e tongue sticking out and our family one day"

Por lo tanto, se usó una aproximación distinta. Primero se eliminaron las apariciones del control `\r` en el archivo .txt que indica separación entre líneas, pero no cambio de turnos (Linea. 17). Posteriormente se separó el texto en líneas individuales usando el control `\n` presente en el archivo txt como indicador para ello (L.26). Luego se eliminaron las líneas vacías (L.30,31). Finalmente se convirtió la transcripción en un dataframe (L.66)


## **2. Transformación de datos**

Se descargaron los lexicones señalados más arriba y posteriormente se tokenizó el dataframe separándolo según las palabras emocionales presentes en este (L69-81). Esto generó 3 objetos, cada uno incluyendo la categorización léxica de acuerdo a las características de cada uno de los lexicones. 

En base a estos objetos se puede empezar a generar gráficos para visualizar los resultados.

## **3. Visualizacion**
Se intentaron diversas aproximaciones para graficar los resultados, las cuáles ayudaron a su vez a visibilizar las limitaciones de este tipo de análisis (ver sección siguiente).
Primero se hizo una tabla con las 3 palabras más comunes por cada emoción categorizada por el lexicón nrc, y posteriormente esta se graficó. Sin embargo, este gráfico fue considerado inapropiado ya que una palabra puede ser agrupada en más de una categoría en una aparición lo que distorsiona entonces los resultados. 

Se consideró más apropiado realizar un gráfico de barras para mostrar la frecuencia de las 4 palabras más comunes para expresar sentimientos positivos y negativos, que se presenta a continuación.

![Grafico de barras que muestra las 4 palabras más usadas para demostrar sentimientos positivos y negativos. En el eje Y se indica la frecuencia de aparición que va desde 0 hasta 129. Las palabras más usadas para demostrar sentimientos positivos son “like”, “Good”, “love” y “right”. Las 4 más usadas para sentimientos negativos son “bad”, “hard”, “crazy” y”exhausted” ](https://github.com/augusto-rp/tareas_curso/blob/master/tarea1_kardashians/grafico_bin.jpeg) “Palabras más comunes para expresar emociones positivas y negativas”



## **4. Conclusiones**
Los análisis demuestran las limitaciones de este paquete para realizar el análisis de las emociones presentes en el capítulo. La principal limitación es la ausencia de consideraciones contextuales y pragmáticas en la categorización de las palabras. Esto es evidente en la asignación de valencias positivas a las palabra “like” que en la mayoría de los casos es usada no para indicar gusto por algo sino como muletilla al hablar.

Además, en el lexicón NRC una misma palabra puede corresponden a múltiples emociones, por ejemplo “god” es considerada como indicando anticipación, jubilo, miedo, confianza y sentimientos positivos. Sin embargo, al ver en detalle la aparición de esta palabra en la mayoría de los casos se trata de la expresión “oh my god”.

Un aprendizaje de esta tarea es entonces, que en caso de usar estos lexicones es importante no depender exclusivamente de ellos para el análisis semánticos de las emociones presentes y la importancia de estar familiarizado con el texto.

Finalmente, es importante señalar que debido a las caracteristicas del archivo de transcripcion original no se logró separar los turnos de habla por hablante. Lo que revela también la relevancia de contar con fuentes de datos apropiadamente formateadas para facilitiar sus análisis.

</details>
