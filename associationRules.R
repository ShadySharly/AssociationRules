# ////////////////////////////////////////////////////////////////////////////////////////////////////// #
# ///////////////////////////////////////////// # ATRIBUTOS # ////////////////////////////////////////// #
# ////////////////////////////////////////////////////////////////////////////////////////////////////// #

library(ggpubr)
library(cowplot)
library(corrplot)
library(factoextra)
library(gridExtra)
library(RColorBrewer)
library(tidyr)

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

#________________________________________________________________________________________________________#
# I. MISSING VALUES

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
# observación que en sus variables presente uno o mas missing values, metodo conocido como 
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


# - Soporte: Fracción de transacciones que contienen al articulo "X"

#                                   Soporte (X) = frecuencia (X) / N