# ---
# title: "Predicción de montos recuperación: Alquimias de Falabella"
# author: "Georgina Ureña Ballestero"
# date: "2023-11-25"
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
library(tidyverse)
library(padr) #When there are missing records for time points where observations were absent, pad will automatically insert these records
library(purrr)
library(xts)
library(zoo)


# ----- PASO 1:

# Se realiza la conexión para la carga del dataset.
connection <- dbConnect(odbc(),
                        Driver = "SQL Server",
                        Server = "200.74.250.96",
                        Database = "XPERSOFT_BD",
                        UID = "gurena",
                        PWD = "CC#E!rMzuWz3gjw@")

# Se obtiene el dataset de interés.
# El análisis sólo se debe realizar para el tipo de cartera Propia, por lo cual se aplica este filtro. 
mex_pagos_falabella_alquimias_data = dbGetQuery(connection, "
                                                SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, ALQUIMIA AS ALQUIMIA,
                                                  FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                                                FROM CRED_PAGOS
                                                WHERE PAI_NOMBRE = 'MEXICO'
                                                  AND TIPO_CARTERA = 'Propia'
                                                  AND INSTITUCION = 'FALABELLA'
                                                ")

# Se cierra la conexión.
dbDisconnect(connection)


# ----- PASO 2: 

# Se preparan los datos.
# str(mex_pagos_falabella_alquimias_data)
mex_pagos_falabella_alquimias_data$FECHA_PAGO = as.Date(mex_pagos_falabella_alquimias_data$FECHA_PAGO,format="%d/%m/%Y")
# summary(mex_pagos_falabella_alquimias_data)

# Se agrupa el total de pagos por fecha, ya que el objetivo es pronosticar de forma global (en este caso) y no por cliente.
mex_pagos_falabella_alquimias = mex_pagos_falabella_alquimias_data %>% 
  group_by(INSTITUCION, ALQUIMIA, FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO))

# # No existe certeza de que cada alquimia cumpla con el requisito de número de observaciones mínimas, por eso se debe conocer del n de cada caso.   
n_alquimias = mex_pagos_falabella_alquimias %>%
  group_by(ALQUIMIA) %>%
  tally() # Counts how many elements are in each group
# # Hay 1 alquimia; además, se debe determinar si las observaciones están en fechas consecutivas.

# Se dejan las observaciones hasta el mes anterior completo actual. Por ejemplo, si la carga del dataset se realiza a mitad de octubre 2023, que tome como último valor aquel que, por alquimia, tenga registro en septiembre 2023.
currentDate = Sys.Date()
end_last_month = rollback(currentDate)
init_last_month = rollback(end_last_month, roll_to_first = TRUE)

alquimias_mes_anterior = mex_pagos_falabella_alquimias %>%
  select(-INSTITUCION) %>%
  filter(FECHA_PAGO <= init_last_month)


# Se necesitan algunos meses más para que la alquimia cumpla con el mínimo de observaciones para poder realizar un análisis de series de tiempo (24).