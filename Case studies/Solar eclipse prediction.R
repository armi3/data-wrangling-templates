#' @title Predicción de eclipse
#' @author Fernanda González (20180190)

#' PREPARAR AMBIENTE DE TRABAJO
install.packages("tidyverse")
library(lubridate)

#' VARIABLES
historic_eclipse <- dmy_hms("21-08-2017 18:26:40")
synodic_month <- (29*ddays(1))+(12*dhours(1))+(44*dminutes(1))+(3*dseconds((1)))
saros_cycle <- 223*synodic_month
next_eclipse <- historic_eclipse + saros_cycle
print(synodic_month)
print(saros_cycle)
print(next_eclipse)