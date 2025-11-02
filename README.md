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
 
## 2. **Transformación de datos**
 
## 3. **Modelar/Visualizar**
 
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
