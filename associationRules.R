# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
# ///////////////////////////////////////////// # LIBRERIAS # ////////////////////////////////////////// #
# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(knitr)
library(ggpubr)
library(cowplot)
library(corrplot)
library(factoextra)
library(gridExtra)
library(RColorBrewer)
library(lubridate)
library(tidyr)

# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
# ///////////////////////////////////////////// # ATRIBUTOS # ////////////////////////////////////////// #
# ////////////////////////////////////////////////////////////////////////////////////////////////////// #

# - code: Numero del código de la muestra
# - clumpThickness: Grosor del grupo (1 - 10)
# - unifCellSize: Tamaño de célula uniforme (1 - 10)
# - unifCellShape: Forma de célula uniforme (1 - 10)
# - marginalAdhesion: Adhesión marginal (1 - 10)
# - epithCellSize: Tamaño de célula epitelial (1 - 10)
# - bareNuclei: Núcleos desnudos (1 - 10)
# - blandChromatin: Cromatina suave (1 - 10)
# - normalNucleoli: Nucleolos normales (1 - 10) 
# - mitoses: Mitosis (1 - 10)
# - class: Clase (2 para BENIGNO, 4 para MALIGNO)

# Se crean los nombres que representaran a cada columna, relativos a los parámetros que son de relevancia
# en cada observación.
columns = c("code",
            "clumpThickness",
            "unifCellSize",
            "unifCellShape",
            "marginalAdhesion",
            "epithCellSize",
            "bareNuclei",
            "blandChromatin",
            "normalNucleoli",
            "mitoses",
            "class"
)

# Se procede a almacenar los datos desde el repositorio web "Breast Cancer Wisconsin" (Original), esto en
# un data frame llamado "df"
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
#url = "breast-cancer-wisconsin.data"
df = read.csv(url, header = F, sep=",", col.names = columns)
set.seed(20)

# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
# ///////////////////////////////////////// # PRE-PROCESAMIENTO # ////////////////////////////////////// #
# ////////////////////////////////////////////////////////////////////////////////////////////////////// #

# Se sabe que el conjunto de datos cuenta con 16 observaciones que presentan missing values para 
# la variable "bareNuclei", denotados por un caracter "?", sin embargo el lenguaje R normalmente
# asocia este tipo de valores con el símbolo "NA" al igual que todos los paquetes relativos a los
# missing values, por lo que para trabajar de mejor manera se procede a cambiar los "?" por "NA".
df.n = nrow(df)
df.m = ncol(df)

for (row in 1:df.n) {
  for (col in 1:df.m) {
    if (df[row, col] == "?") {
      df[row, col] <- NA
    }
  }
}

# Debido a que la variable bareNuclei contenía valores "?" la variable esta clasificada como de tipo
# "character". por lo que es necesario modificarla para que sea del tipo "integer".
df$bareNuclei <- as.integer(df$bareNuclei)

# Una de las formas de manejar los valores omitidos, consiste en simplemente eliminar cada
# observación que en sus variables presente uno o mas missing values, método conocido como 
# "Listwise Deletion", ahora bien la simplicidad de este método viene perjudicado por el hecho de que el
# modelo pierde fuerza, ya que se pierde información relevante, ahora bien dependiendo de la razón entre
# el numero de observaciones que presentan missing values y el total de observaciones, puede afectar en
# menor o mayor medida la precisión del modelo para predecir una variable de estudio. En este caso, razón
# de observaciones que se perderían al aplicar este método corresponde a un 2.3% aproximadamente,
# un numero bastante bajo para considerar este método.
df <- na.omit(df)

# Almacenamos el conjunto de datos original, en caso de realizar modificaciones posteriores y no 
# afectar los datos del conjunto inicial, y necesitar estos datos originales.
df.original <- df

# Quitamos la primera variable ya que el código no aporta ninguna información de interés para la creación
# de reglas

df <- subset(df, select = -c(code))

# Ahora dado que el dominio para cada una de las variables es discreto, y que estos van desde el 1 al 10, 
# es posible convertir cada una de estas variables enteras, a variables de tipo "factor", a excepción
# de la variable "class", la cual tiene un comportamiento distinto tomando solo dos valores 2 y 4.

df[, 1:9] = lapply(df[, 1:9], factor)

df$class = factor(
  df$class, 
  levels = c(2, 4), 
  labels = c("Benigno", "Maligno"))

# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
# //////////////////////////////////////// # REGLAS DE ASOCIACION # //////////////////////////////////// #
# ////////////////////////////////////////////////////////////////////////////////////////////////////// #

# La minería de Reglas de Asociación es utilizada cuando se busca encontrar una asociación entre
# diferentes objetos en un conjunto de datos, tales como patrones que se repiten con frecuencia en 
# una base de datos transaccional, bases de datos relacionales u algún otro tipo de repositorio de 
# información como lo es este caso. Estas reglas están fundamento en el "marketing", en el denominado
# "Análisis de la Canasta de Mercado", en el "retail", "clustering" y "clasificación". Es útil para 
# identificar que artículos son comprados frecuentemente y de manera conjunta por los clientes, a 
# través de la generación de un conjunto de reglas denominadas como "Reglas de Asociación".

# Aplicando los fundamentos de las reglas de asociación, en el contexto del estudio presente, como
# primicia se puede decir que estas permiten entre otras cosas, determinar que variables de estudio
# en las muestras de tejido recopiladas por un hospital X, tienen tendencia a tomar cierto rango de 
# valores, considerando los valores de otras variables. Aun mas específicamente, considerando
# la variable explicativa "class", podría determinarse que un conjunto de características en las
# células de las muestras de tejidos observadas, tienden a mostrar un determinado patrón que
# podría incidir en el valor que toma la variable explicativa, u otra variable de la muestra.

# Una Regla de Asociación se estructura de la siguiente manera:

#                                     A => B [Soporte, Confidencia]

# A: Corresponde al ANTECEDENTE de la regla, y hace referencia a la palabra "SI"
# B: Corresponde al CONSECUENTE de la regla, y hace referencia a la palabra "ENTONCES"


#________________________________________________________________________________________________________#
# I. CONCEPTOS BÁSICOS

#################
# a. Soporte    #
#################

# Fracción de transacciones que contienen al articulo "X"
 
#                               =========================================
#                               ==  Soporte (X) = frecuencia (X) / N   ==
#                               =========================================
                               
# Para una regla A => B, el soporte es definido como :
  
#                         ====================================================
#                         ==    Soporte (A => B) = frecuencia (A, B) / N    ==
#                         ====================================================

###################
# b. Confianza    #
###################

# Para una regla A => B el soporte explica la probabilidad de que al escoger el ítem A, se escoja el ítem
# B, y matemáticamente se calcula como el numero total de transacciones que implica a "A" y "B", dividido
# por el total de transacciones que implican a "A", tal como se muestra a continuación:

#                           =================================================             
#                           ==    Confianza (A => B) = P(A ∩ B) / P(A)     ==  
#                           =================================================  

#                    =============================================================== 
#                    ==  Confianza (A => B) = frecuencia (A, B) / frecuencia (A)  ==
#                    ===============================================================

#                      ========================================================== 
#                      ==  Confianza (A => B) = Soporte (A ∩ B) / Soporte (A)  ==
#                      ==========================================================

# *********************************************************************************************************
# * Tanto el Soporte como la Confianza determinan cuan INTERESANTE es una regla, parámetro el cual es     *
# * medido en base a un umbral de interés, entonces mientras mas cerca estén estos valores de ese umbral, *
# * mas útil es la regla para el cliente (en el contexto de la canasta de mercado). Una regla INTERSEANTE *
# * es aquella que es FRECUENTE y CONFIABLE a la vez, donde:                                              *
# *                                                                                                       *
# * -> El conjunto de reglas CONFIABLES se entiende que es el conjunto de todas las reglas que cumplen    *
# *    con una CONFIANZA MÍNIMA "minconf"                                                                 *
# *                                                                                                       *
# * -> El conjunto de reglas FRECUENTES se entiende que es el conjunto de todas las reglas que cumplen    *
# *    con un SOPORTE MÍNIMO "minsop"                                                                     *
# *********************************************************************************************************


#________________________________________________________________________________________________________#
# II. MEDIDAS DE CALIDAD



#________________________________________________________________________________________________________#
# III. ALGORITMO APRIORI

# La Minería de Reglas de Asociación considera dos pasos fundamentales, los cuales son:

# 1. Generación de Conjuntos Frecuentes: Encontrar todos los conjunto de items FRECUENTES, que cuenten
#    con un SOPORTE >= minsop.

# 2. Generación de Reglas: Listar todas las Reglas de Asociación para los Conjuntos Frecuentes. Luego 
#    determinar el soporte y la confianza para todas las reglas. Finalmente eliminar aquellas reglas que 
#    están por debajo de "minsop" y "minconf"

# Para aplicar el algoritmo anterior en R, existe una función dedicada a esta tarea llamada "apriori",
# función en la cual es necesario desde luego especificar los umbrales anteriormente mencionados,
# relativos tanto al soporte mínimo, como a la confianza mínima. Así también es posible restringir
# el CONSECUENTE de las reglas que se generaran, a una característica especifica del conjunto de datos, y
# que desde luego depende de la naturaleza del problema, y de que relaciones dan valor al problema de 
# estudio.

# Considerando el estudio bajo el cual se recabaron los datos, seria interesante determinar la relación 
# existente a partir de la variable "class" dada (2 = Benigno, 4 = Maligno), junto con las demás 
# características del conjunto de datos. En el ámbito medico, seria posible determinar el diagnostico 
# de cáncer de una persona, a partir de las características las células pertenecientes a una muestra de
# tejido, en cuanto a que valores toman en una determinada métrica.

# Entonces dado lo anterior, el CONSECUENTE se establece como la variable "class", del conjunto de datos, 
# y se especifica en el parámetro "appeareance"

rules.benign = apriori(
  data = df, 
  parameter = list(support = 0.2, conf = 0.8, minlen = 2, maxlen = 10, target = "rules"),
  appearance = list(rhs = c("class=Benigno"))
)

summary(rules.benign)

rules.malign = apriori(
  data = df, 
  parameter = list(support = 0.2, conf = 0.8, minlen = 2, maxlen = 10, target = "rules"),
  appearance = list(rhs = c("class=Maligno"))
)

summary(rules.malign)

rules = apriori(
  data = df, 
  parameter = list(support = 0.2, conf = 0.8, minlen = 2, maxlen = 10, target = "rules"),
  appearance = list(rhs = c("class=Benigno", "class=Maligno"))
)

summary(rules)

inspect(sort(x = rules.malign, decreasing = TRUE, by = "confidence"))


# Eliminacion de Reglas Redundantes

subset.benign <- which(colSums(is.subset(rules.benign, rules.benign)) > 1)
subset.malign <- which(colSums(is.subset(rules.malign, rules.malign)) > 1)
length(subset.benign)
length(subset.malign)

# subset.association.rules. <- association.rules[-subset.rules]















                         