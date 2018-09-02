#clase 01

#asignar archivo a variable
texto <- readLines("quijote.txt")

texto <- readLines("quijote.txt", encoding = "UTF-8")

texto <- readLines("quijote.txt", encoding = "UTF-8", n = 10)

texto <- readLines("quijote.txt", encoding = "UTF-8", n = 10, skipNul = TRUE)

#Info de funcion
?readLines


#instalar lib
install.packages("readr")

#llamar libreria
library(readr)

#leer lineas, ya trae utf-8
quijote_lines <- read_lines("quijote.txt")

#ver header
head(quijote_lines)

#imprimir num lineas
quijote_lines[10:20]

#asiganr a variable el path 
text_file <- "quijote.txt"
read_lines(text_file, skip=10)


################### sentiment analysis exercise #############
install.packages("tidytext")
install.packages("tidyverse")

library(tidytext)
library(tidyverse)

quijote_frame <- data_frame(txt=quijote_lines)
quijote_frame

?unnest_tokens

#tokenizar
quijote_words <- unnest_tokens(quijote_frame, input = txt, output = word, token = "words")
quijote_words

#contar tokens
quijote_count <- count(quijote_words, word, sort = TRUE)
quijote_count


#librerias recomendadas
library(queanteda)
library(dplyr)


####### JUEVES 26 ##########

# importar archivo
hour <- read.csv('hour.csv')
head(hour)

# ver los nombres de columnas
names(hour)

# ver solo unas columnas
hour[c("temp")]

# extraer columnas por no. de columna
hour[,c(3)]

# asignar columnas especificas a una variable para crear otro dataset
columnas <- hour[c("hr","temp")]
head(columnas)

#exportar archivo
write.csv(columnas, file="exportedFile.csv")


###### Leer excels
install.packages("readxl")
library (readxl)

# leer archivo
bancos <- read_excel("bancos.xlsx")

# leer archivo, solo una hoja en particular
bancos_sheet2 <- read_excel("bancos.xlsx", sheet="agencias")

