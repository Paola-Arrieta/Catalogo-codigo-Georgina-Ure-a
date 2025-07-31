# ---
# title: "Predicción de montos recuperación Credimovil"
# author: "Georgina Ureña Ballestero"
# date: "2023-10-05"
# output: html_document
# ---
  
# Bibliotecas
library(DBI)
library(dplyr)
library(fable)
library(forecast)
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
cr_pagos_credimovil_data = dbGetQuery(connection, "
                                 SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, 
                                  FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                                 FROM CRED_PAGOS
                                 WHERE PAI_NOMBRE = 'COSTA RICA' 
                                  AND TIPO_CARTERA = 'Propia'
                                  AND INSTITUCION = 'CREDIMOVIL' 
                                  AND MONTO_RECIBO >= 5000.000
                                 ")

# Se cierra la conexión.
dbDisconnect(connection)


# Preparación de los datos

# str(cr_pagos_credimovil_data)
cr_pagos_credimovil_data$FECHA_PAGO = as.Date(cr_pagos_credimovil_data$FECHA_PAGO,format="%d/%m/%Y")
# summary(cr_pagos_credimovil_data)
# # Se tienen observaciones desde 2001-10hasta 2013-05(no necesariamente equiespaciadas o consecutivas, por lo que hay que cerciorarse de que esta condición se cumpla; de no ser así se puede plantear recurrir a la imputación).

cr_pagos_credimovil = cr_pagos_credimovil_data %>% 
  group_by(INSTITUCION, FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO)) %>% 
  # Add missing dates per month
  pad() %>%
  as.data.frame()

# # Determinación del porcentaje de valores vacíos respecto del total de datos
# # para cada columna
# porcentaje_empty = function(x) {sum(is.na(x)) / length(x)*100}
# # por columna
# apply(cr_pagos_credimovil, 2, porcentaje_empty)
# # 65.7% de la data en la que no hubo recuperación. Ante esto, decido no realizar el análisis; además, tiene más de 10 años en que no se registra recuperación.
