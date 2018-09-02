#' @title Reporte para Inversiones en Energía, S.A. de C.V. 
#' @author Fernanda González (20180190)

#' PREPARAR AMBIENTE DE TRABAJO
install.packages("tidyverse")
library(tidyverse)
library(lubridate)

#' IMPORTAR DATOS
df <- read.csv("/Users/baroness/Documents/201802/CS-DS001 Data Wrangling (A)/Lab04/c1.csv")

#' HACER TIDY
df <- df[1:22] %>%
  dplyr::mutate_all(toupper) %>%
  dplyr::mutate_all(str_trim) %>%
  dplyr::na_if("") %>%
  tidyr::gather(key="DURACIÓN", value="FRECUENCIA", 18:22, na.rm=TRUE)

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

unique(df$CÓDIGO)
df <- df %>%
  dplyr::mutate("CÓDIGO" = dplyr::case_when(Cod=="REVISION_TRANSFORMADOR" ~ "PREVENTIVO",
                                          Cod=="REVISION" ~ "PREVENTIVO",
                                          Cod=="VERIFICACION_INDICADORES" ~ "PREVENTIVO",
                                          Cod=="VERIFICACION_MEDIDORES" ~ "PREVENTIVO",
                                          Cod=="VISITA_POR_CORRECCION" ~ "CORRECTIVO",
                                          Cod=="CAMBIO_CORRECTIVO" ~ "CORRECTIVO",
                                          Cod=="CAMBIO_FUSIBLE" ~ "CORRECTIVO",
                                          Cod=="CAMBIO_PUENTES" ~ "CORRECTIVO",
                                          Cod=="OTRO" ~ "OTRO",
                                          Cod=="VISITA" ~ "OTRO",
                                                   TRUE ~ Cod))%>%
  dplyr::select(-Cod)

#' TRANSFORMAR
servicios_mensuales <- df%>%
  dplyr::select(Fecha, CÓDIGO)%>%
  dplyr::group_by(MES=lubridate::month(Fecha, label=TRUE))%>%
  dplyr::count(CÓDIGO)%>%
  tidyr::spread(MES, n)
head(servicios_mensuales)

servicios_diarios <- df%>%
  dplyr::select(Fecha, CÓDIGO)%>%
  dplyr::group_by(WEEKDAY=lubridate::wday(Fecha, label=TRUE))%>%
  dplyr::count(CÓDIGO)%>%
  tidyr::spread(WEEKDAY, n)
head(servicios_diarios)

services_turnover <- df%>%
  dplyr::select(Fecha, CÓDIGO)%>%
  dplyr::group_by(CÓDIGO)%>%
  dplyr::count()%>%
  dplyr::mutate(TURNOVER=n/365)
head(services_turnover)

mov_demanda <- df%>%
  dplyr::select(Fecha, CÓDIGO)%>%
  dplyr::mutate(SEMANA=lubridate::floor_date(dmy(Fecha), "8 days"))%>%
  dplyr::group_by(MES=lubridate::month(Fecha, label=TRUE))%>%
  dplyr::summarise("W1"=sum(lubridate::day(SEMANA)==1),
                   "W2"=sum(lubridate::day(SEMANA)==9),
                   "W3"=sum(lubridate::day(SEMANA)==17),
                   "W4"=sum(lubridate::day(SEMANA)==25))
View(mov_demanda)

write.csv(servicios_mensuales,"servicios_mensuales.csv")
write.csv(servicios_diarios,"servicios_diarios.csv")
write.csv(services_turnover,"services_turnover.csv")
write.csv(mov_demanda,"mov_demanda.csv")