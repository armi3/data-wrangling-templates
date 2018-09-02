#' @title Reporte para Inversiones en Energía, S.A. de C.V. 
#' @author Fernanda González (20180190)
#' @description Preguntas a responder:
#' 
#' PREPARAR AMBIENTE DE TRABAJO
install.packages("tidyverse")
library(tidyverse)

#' IMPORTAR DATOS
df <- read.csv("/Users/baroness/Documents/201802/CS-DS001 Data Wrangling (A)/Lab04/c1.csv")

#' EXPLORAR
View(df)

#' HACER TIDY
df <- df[1:22] %>%
  dplyr::mutate_all(toupper) %>%
  dplyr::mutate_all(str_trim) %>%
  dplyr::na_if("") %>%
  tidyr::gather(key="DURACIÓN", value="FRECUENCIA", 18:22, na.rm=TRUE)
View(df)

df <- df[1:18] %>%
  dplyr::mutate(Moto = as.numeric(gsub("Q", "", df$Moto)),
                Pickup = as.numeric(gsub("Q", "", df$Pickup)),
                Camion_5 = as.numeric(gsub("Q", "", df$Camion_5)),
                fijoMoto = as.numeric(gsub("Q", "", df$fijoMoto)),
                fijoPickup = as.numeric(gsub("Q", "", df$fijoPickup)),
                fijoCamion_5 = as.numeric(gsub("Q", "", df$fijoCamion_5)),
                directoMoto = as.numeric(gsub("Q", "", df$directoMoto)),
                directoPickup = as.numeric(gsub("Q", "", df$directoPickup)),
                directoCamion_5 = as.numeric(gsub("Q", "", df$directoCamion_5)),
                factura = as.numeric(gsub("Q", "", df$factura)),
                DURACIÓN = as.character(gsub("X", "", df$DURACIÓN))) %>%
  tidyr::gather(key="VEHÍCULO", value="COSTO OPERACIONAL TOTAL", 3:5, na.rm=TRUE) %>%
  tidyr::gather(key="DV", value="COSTO OPERACIONAL DIRECTO", directoCamion_5:directoMoto, na.rm=TRUE) %>%
  tidyr::gather(key="DF", value="COSTO OPERACIONAL FIJO", fijoCamion_5:fijoMoto, na.rm=TRUE)

df$DV<-NULL
df$DF<-NULL

df <- tidyr::separate(df, DURACIÓN, c('DUR MIN', 'DUR MÁX'), sep="([.])+")  

#' TRANSFORMAR
write.csv(df,"df.csv")

#' VISUALIZE
#' MODEL
#' COMMUNICATE