# ---
# title: "Predicción de montos recuperación Multibank"
# author: "Georgina Ureña Ballestero"
# date: "2023-11-21"
# output: html_document
# ---
  
# Bibliotecas
library(DBI)
library(dplyr)
library(DT)
library(dygraphs)
library(fable)
library(forecast)
library(fmsb)
library(fpp2)
library(ggplot2)
library(lubridate)
library(nortest)
library(odbc)
library(padr) #When there are missing records for time points where observations were absent, pad will automatically insert these records
library(purrr)
library(reshape)
library(tidyr)
library(tidyverse)
library(TSA)
library(tsibble)
library(xts)
library(zoo)


# Se realiza la conexión para la carga del dataset.
connection <- dbConnect(odbc(),
                        Driver = "SQL Server",
                        Server = "200.74.250.96",
                        Database = "XPERSOFT_BD",
                        UID = "gurena",
                        PWD = "CC#E!rMzuWz3gjw@")

# Se obtiene el dataset de interés.
pan_pagos_multibank_data = dbGetQuery(connection, "
                                 SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, 
                                  FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                                 FROM CRED_PAGOS
                                 WHERE PAI_NOMBRE = 'PANAMA'
                                  AND TIPO_CARTERA = 'Propia'
                                  AND INSTITUCION = 'MULTIBANK' 
                                  ")

# Se cierra la conexión.
dbDisconnect(connection)


# Preparación de los datos

# str(pan_pagos_multibank_data)
pan_pagos_multibank_data$FECHA_PAGO = as.Date(pan_pagos_multibank_data$FECHA_PAGO,format="%d/%m/%Y")
# summary(pan_pagos_multibank_data)
# # Se tienen observaciones desde 2022-068 hasta 2023 (no necesariamente equiespaciadas o consecutivas, por lo que hay que cerciorarse de que esta condición se cumpla; de no ser así se puede plantear recurrir a la imputación).

pan_pagos_multibank = pan_pagos_multibank_data %>% 
  group_by(INSTITUCION, FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO)) %>% 
  # Add missing dates per month
  pad() %>%
  as.data.frame()

# # Determinación del porcentaje de valores vacíos respecto del total de datos
# # para cada columna
# porcentaje_empty = function(x) {sum(is.na(x)) / length(x)*100}
# # por columna
# apply(pan_pagos_multibank, 2, porcentaje_empty)
# # 18.75% de la data en la que no hubo recuperación.


# Se necesitan algunos meses más para que la institución cumpla con el mínimo de observaciones para poder realizar un análisis de series de tiempo (24).