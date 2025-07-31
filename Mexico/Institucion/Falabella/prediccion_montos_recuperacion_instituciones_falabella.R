# ---
# title: "Predicción de montos recuperación Falabella"
# author: "Georgina Ureña Ballestero"
# date: "2023-11-25"
# output: html_document
# ---
  
# Bibliotecas
library(DBI)
library(dplyr)
library(fable)
library(forecast)
library(ggplot2)
library(lubridate)
library(odbc)
library(tsibble)
library(tidyr)
library(padr) #When there are missing records for time points where observations were absent, pad will automatically insert these records
library(purrr)
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
mex_pagos_falabella_data = dbGetQuery(connection, "
                                 SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, 
                                  FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                                 FROM CRED_PAGOS
                                 WHERE PAI_NOMBRE = 'MEXICO'
                                  AND TIPO_CARTERA = 'Propia'
                                  AND INSTITUCION = 'FALABELLA' 
                                  ")

# Se cierra la conexión.
dbDisconnect(connection)


# Preparación de los datos

# str(mex_pagos_falabella_data)
mex_pagos_falabella_data$FECHA_PAGO = as.Date(mex_pagos_falabella_data$FECHA_PAGO,format="%d/%m/%Y")
# summary(mex_pagos_falabella_data)
# # Se tienen observaciones desde 2022-09 hasta 2023 (no necesariamente equiespaciadas o consecutivas, por lo que hay que cerciorarse de que esta condición se cumpla; de no ser así se puede plantear recurrir a la imputación).

mex_pagos_falabella = mex_pagos_falabella_data %>% 
  group_by(INSTITUCION, FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO)) %>% 
  # Add missing dates per month
  pad() %>%
  as.data.frame()

# Se necesitan algunos meses más para que la institución cumpla con las observaciones mínimas para poder realizar un análisis de series de tiempo (24).