#' @title Reporte para Distribuidora del Sur, S.A.
#' @author Fernanda González (20180190)
#' @description Preguntas a responder:
#' 1) ¿Contratar más personal?
#' 2) ¿Comprar más vehículos? ¿Cuáles?
#' 3) ¿Están bien las tarifas actuales?
#' 4) ¿Roban los pilotos?
#' 5) 80-20 de clientes
#' 6) 80-20 de pilotos

#' PREPARAR AMBIENTE DE TRABAJO
install.packages("tidyverse")
library(tidyverse)

#' IMPORTAR DATOS
Entregas2017 <- read.csv("/Users/baroness/Documents/201802/CS-DS001 Data Wrangling (A)/Lab03/Reporte2017/Entregas2017.csv")

#' EXPLORAR
summary(Entregas2017)

#' HACER TIDY
Entregas2017<-Entregas2017 %>%
  tidyr::separate(CLIENTE, c('CLIENTE', 'ESTATUS_ENTREGA', 'ESTATUS_ENTREGA2'), sep="([/|])+")%>%
  #' Algunos clientes y estatus quedaron con whitespaces al final del nombre.
  dplyr::mutate(CLIENTE = str_trim(CLIENTE)) %>%
  dplyr::mutate(ESTATUS_ENTREGA = str_trim(ESTATUS_ENTREGA)) %>%
  dplyr::mutate_all(toupper) %>%
  #' Todas las entregas que reportaron producto faltante fueron primero despachadas. 
  #' Registrarlas como despachadas y faltante es redundante.
  dplyr::mutate(ESTATUS_ENTREGA = dplyr::case_when(ESTATUS_ENTREGA2=="FALTANTE" ~ "FALTANTE", 
                                                   TRUE ~ ESTATUS_ENTREGA)) %>%
  dplyr::select(-X, -COD_VIAJE, -ESTATUS_ENTREGA2) %>%
  dplyr::mutate(UBICACION = dplyr::case_when(UBICACION==76001 ~ 1, 
                                             UBICACION==76002 ~ 2, 
                                             TRUE ~ as.numeric(UBICACION)))
head(Entregas2017)

#' TRANSFORM
#' 1) Para ver si las tarifas están bien y los 80-20 clientes.
#' 1.1) Total de pedidos, total por ubicacion, total de clientes, unidades y Q, crédito promedio.
Resumen2017<-Entregas2017%>%
  dplyr::select(Fecha, UBICACION, CLIENTE, CANTIDAD, Q, CREDITO) %>%
  dplyr::summarise(PEDIDOS=sum(n()),
                   CLIENTES=n_distinct(CLIENTE),
                   CLIENTES20=n_distinct(CLIENTE)*0.2,
                   UBI1=sum(UBICACION== 1),
                   UBI2=sum(UBICACION== 2),
                   PRODUCTO=sum(as.numeric(CANTIDAD)),
                   MEDIA_CRED=mean(as.numeric(CREDITO)),
                   Q_TOTAL=sum(as.numeric(Q)),
                   Q80=sum(as.numeric(Q))*0.8)
head(Resumen2017)
  
#' 1.2) Faltante/devolución/despacho por cliente.
Entregas2017$ESTATUS_ENTREGA<-as.factor(Entregas2017$ESTATUS_ENTREGA)

Clientes2017<-Entregas2017 %>%
  dplyr::group_by(CLIENTE) %>%
  dplyr::count(ESTATUS_ENTREGA) %>%
  tidyr::spread(key=ESTATUS_ENTREGA, value=n)%>%
  dplyr::arrange(CLIENTE)
head(Clientes2017)

Clientes2017pt2<-Entregas2017 %>%
  dplyr::select(CLIENTE, CANTIDAD, Q, CREDITO) %>%
  dplyr::group_by(CLIENTE) %>%
  dplyr::summarise(CANTIDAD=sum(as.numeric(CANTIDAD)),
                   Q=sum(as.numeric(Q)),
                   CREDITO=mean(as.numeric(CREDITO))) %>%
  dplyr::arrange(CLIENTE)
head(Clientes2017pt2)

Clientes2017<-merge(x=Clientes2017,
                    y=Clientes2017pt2[, c("CLIENTE","CANTIDAD", "Q", "CREDITO")], 
                    by="CLIENTE", all.x = TRUE)
Clientes2017[is.na(Clientes2017)]<-0
head(Clientes2017)

ClientesCrono <- Entregas2017 %>%
  dplyr::select(Fecha, CLIENTE, ESTATUS_ENTREGA) %>%
  dplyr::group_by(Fecha, CLIENTE) %>%
  dplyr::count(ESTATUS_ENTREGA) %>%
  tidyr::spread(key=ESTATUS_ENTREGA, value=n) %>%
  dplyr::arrange(CLIENTE)
ClientesCrono[is.na(ClientesCrono)]<-0
head(ClientesCrono)

ClientesCronoPt2 <- Entregas2017 %>%
  dplyr::select(Fecha, CLIENTE, Q) %>%
  dplyr::group_by(Fecha, CLIENTE) %>%
  dplyr::summarise(Q=sum(as.numeric(Q))) %>%
  dplyr::arrange(CLIENTE)
head(ClientesCronoPt2)

ClientesCrono<-ClientesCronoPt2 %>%
  dplyr::select(Q) %>%
  dplyr::bind_cols(ClientesCrono)
head(ClientesCrono)

ClientesCrono<-subset(ClientesCrono, select=c(Fecha, CLIENTE, Q, `DESPACHO A CLIENTE`,`DEVOLUCION`,`FALTANTE`,`<NA>`))

#' 2) Para encontrar 80-20 pilotos, si roban, necesidad de más pilotos/vehículos.
#' 2.1) Resumen de pilotos
ResumenPilotos <- Entregas2017 %>%
  dplyr::select(Fecha, PILOTO, UNIDAD) %>%
  dplyr::summarise(PILOTOS=n_distinct(PILOTO),
                   PILOTOS20=n_distinct(PILOTO)*0.2,
                   ENTREGA_PANEL=sum(str_count(UNIDAD, "PANEL")),
                   ENTREGA_GRANDE=sum(str_count(UNIDAD, "CAMION GRANDE")),
                   ENTREGA_PEQUE=sum(str_count(UNIDAD, "CAMION PEQUENO")))
head(ResumenPilotos)

#' 2.2) Sábana de pilotos.
#' Entregas por hora por piloto 
Entregas2017$ESTATUS_ENTREGA<-as.factor(Entregas2017$ESTATUS_ENTREGA)
Pilotos2017<-Entregas2017 %>%
  dplyr::group_by(PILOTO) %>%
  dplyr::count(ESTATUS_ENTREGA) %>%
  tidyr::spread(key=ESTATUS_ENTREGA, value=n)%>%
  dplyr::arrange(PILOTO)
head(Pilotos2017)

#' 2.3) Tipos de vehículo
#' Entregas por hora por tipo de vehículo 
Vehiculos<-Entregas2017 %>%
  dplyr::group_by(PILOTO, UNIDAD) %>%
  dplyr::count(ESTATUS_ENTREGA) %>%
  tidyr::spread(key=ESTATUS_ENTREGA, value=n)%>%
  dplyr::arrange(PILOTO)
head(Vehiculos)

Vehiculos2<-Entregas2017%>%
  dplyr::group_by(UNIDAD)%>%
  dplyr::summarise(Q=sum(as.numeric(Q)))
head(Vehiculos2)

#' VISUALIZE
#' MODEL
#' COMMUNICATE
write.csv(Clientes2017,"Clientes2017.csv")
write.csv(ClientesCrono, "ClientesCrono.csv")
write.csv(Pilotos2017,"Pilotos2017.csv")
write.csv(Entregas2017,"Entregas2017_TIDY.csv")
write.csv(Vehiculos,"Vehiculos.csv")
write.csv(Vehiculos2,"Vehiculos2.csv")
