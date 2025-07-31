# ---
# title: "Predicción de montos recuperación: Alquimias de Promérica"
# author: "Georgina Ureña Ballestero"
# date: "2023-10-30"
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
# El análisis sólo se debe realizar para el tipo de cartera Propia, por lo cual se aplica este filtro. Adicionalmente, por indicaciones de Don Willie, se deben eliminar montos menores a 5000. 
cr_pagos_promerica_alquimias_data = dbGetQuery(connection, "
                                               SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, ALQUIMIA AS ALQUIMIA, 
                                                FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                                               FROM CRED_PAGOS
                                               WHERE PAI_NOMBRE = 'COSTA RICA'
                                                AND TIPO_CARTERA = 'Propia'
                                                AND INSTITUCION = 'A-PROMERICA'
                                               AND MONTO_RECIBO >= 5000.000
                                               ")

# Se cierra la conexión.
dbDisconnect(connection)


# ----- PASO 2: 

# Se preparan los datos.
# str(cr_pagos_promerica_alquimias_data)
cr_pagos_promerica_alquimias_data$FECHA_PAGO = as.Date(cr_pagos_promerica_alquimias_data$FECHA_PAGO,format="%d/%m/%Y")
# summary(cr_pagos_promerica_alquimias_data)

# Se agrupa el total de pagos por fecha, ya que el objetivo es pronosticar de forma global (en este caso) y no por cliente.
cr_pagos_promerica_alquimias = cr_pagos_promerica_alquimias_data %>% 
  group_by(INSTITUCION, ALQUIMIA, FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO))

# # No existe certeza de que cada alquimia cumpla con el requisito de número de observaciones mínimas, por eso se debe conocer del n de cada caso.   
# n_alquimias = cr_pagos_promerica_alquimias %>%
#   group_by(ALQUIMIA) %>%
#   tally() # Counts how many elements are in each group
# # Hay 194 alquimias; además, se debe determinar si las observaciones están en fechas consecutivas.

# Se dejan las observaciones hasta el mes anterior completo actual. Por ejemplo, si la carga del dataset se realiza a mitad de octubre 2023, que tome como último valor aquel que, por alquimia, tenga registro en septiembre 2023.
currentDate = Sys.Date()
end_last_month = rollback(currentDate)
init_last_month = rollback(end_last_month, roll_to_first = TRUE)

alquimias_mes_anterior = cr_pagos_promerica_alquimias %>%
  select(-INSTITUCION) %>%
  filter(FECHA_PAGO <= init_last_month)


# ----- PASO 3: 

# Se seleccionan las alquimias con al menos 24 observaciones, establecido como requisito mínimo para realizar las time series (los registros no son necesariamente en fechas consecutivas).
alquimias_filtradas = alquimias_mes_anterior %>%
  group_by(ALQUIMIA) %>%
  filter(n() >= 24)

# Se agregan las fechas que hacen falta por alquimia para tener observaciones consecutivas.
alquimias_consecutivas = alquimias_filtradas %>%
  group_by(ALQUIMIA) %>%
  complete(FECHA_PAGO = seq.Date(min(FECHA_PAGO), max(FECHA_PAGO), by = "1 month"))

# Se cuentan cuántos NA's hay por alquimia, sólo para conocer el dato.
# na_alquimias = alquimias_consecutivas %>%
#   group_by(ALQUIMIA) %>%
#   tally(is.na(MONTO_TOTAL))
# # 10 alquimias con más de 10 fechas vacías y 5 de ellas tienen más de 30 NA's.

# Se interpolan los valores faltantes.
alquimias_interpoladas = alquimias_consecutivas %>%
  group_by(ALQUIMIA) %>%
  mutate(MONTO_TOTAL=zoo::na.approx(MONTO_TOTAL)) %>%
  ungroup()


# ----- PASO 4: 

# Se crean las series de tiempo para cada alquimia.
# Se utiliza nest para agrupar los datos por ALQUIMIA y luego se crea una lista de objetos xts para cada alquimia utilizando map. El resultado es una tabla que contiene las alquimias junto con sus respectivas series de tiempo en formato ts.
alquimias_series_tiempo = alquimias_interpoladas %>%
  group_by(ALQUIMIA) %>%
  nest() %>%
  mutate(
    serie_tiempo = map(data, ~ {
      fechas <- as.Date(.x$FECHA_PAGO)
      ts(.x$MONTO_TOTAL, start = c(min(fechas), max(fechas)), frequency = 12)
    })
  ) %>%
  select(ALQUIMIA, serie_tiempo) %>%
  ungroup()


# ----- PASO 5: 

# Se ajustan modelos Holt-Winters y se almacenan en una lista para cada alquimia.
alquimias_modelos_hw = alquimias_series_tiempo %>%
  mutate(modelo_hw = map(serie_tiempo, ~ HoltWinters(.x))) %>%
  select(ALQUIMIA, modelo_hw)


# ----- PASO 6: 

# Se generan las predicciones con un horizonte de 6 meses y los intervalos de confianza para cada una de las alquimias.
horizonte = 6

# Se generan las predicciones con un horizonte de 6 meses y los intervalos de confianza para cada una de las alquimias.
predicciones_hw_lista = alquimias_modelos_hw %>%
  mutate(
    predicciones = map(modelo_hw, ~ forecast(.x, h = horizonte, level = c(95)))
  ) %>%
  mutate(
    predicciones_df = map2(predicciones, alquimias_series_tiempo$serie_tiempo, ~ {
      preds <- .x
      preds_mean <- as.numeric(preds$mean)
      alquimia <- tail(index(.y), 1)  # Obtener la última fecha de la serie de tiempo
      fecha_inicio_predicción <- as.Date(tail(index(.y), 1)) + 1 # Comenzar desde el siguiente mes
      fechas_predicción <- seq.Date(fecha_inicio_predicción, by = "1 month", length.out = horizonte)
      data.frame(Alquimia = rep(.y[1], horizonte),  # Acceder a ALQUIMIA mediante .y[1]
                 Fecha_estimacion = fechas_predicción,
                 Estimacion_puntual = preds_mean,
                 Limite_confianza_inferior = as.numeric(preds$lower[,1]),
                 Limite_confianza_superior = as.numeric(preds$upper[,1])
      )
    })
  ) %>%
  select(ALQUIMIA, predicciones_df)

predicciones_hw_tabla = predicciones_hw_lista %>%
  unnest() %>%
  as.data.frame() %>%
  mutate(Limite_confianza_inferior = replace(Limite_confianza_inferior, Limite_confianza_inferior < 0, 0))

# Obtener la última fecha de pago para cada alquimia para calcular su respectivo horizonte de 6 meses 
fechas_prediccion = alquimias_interpoladas %>%
  group_by(ALQUIMIA) %>%
  summarize(ULTIMA_FECHA_PAGO = max(FECHA_PAGO)) %>%
# Agregar 6 fechas más con un horizonte de 6 meses a partir de la última fecha de pago   
  mutate(Fecha = map(ULTIMA_FECHA_PAGO, ~ seq.Date(.x + months(1), by = "1 month", length.out = 6))) %>%
  rename(ALQUIMIA_FECHAS = ALQUIMIA) %>%
  unnest(Fecha)
  
predicciones_finales = cbind(fechas_prediccion,predicciones_hw_tabla) %>%
  select(ALQUIMIA, Fecha, Estimacion_puntual, Limite_confianza_inferior, Limite_confianza_superior) %>%
  as.data.frame()

predicciones_finales_promerica_alquimias = bind_rows(alquimias_mes_anterior, predicciones_finales) %>%
  arrange(ALQUIMIA) %>%
  unite(Fecha_prueba, c(FECHA_PAGO, Fecha), na.rm = TRUE) %>%
  rename(Alquimia=ALQUIMIA, Fecha=Fecha_prueba, Monto_real=MONTO_TOTAL) %>%
  as.data.frame() %>%
  select(Alquimia,Fecha,Monto_real,Estimacion_puntual,Limite_confianza_inferior,Limite_confianza_superior) %>%
  arrange(Alquimia)

# Guardar los resultados en formato RDS
saveRDS(predicciones_finales_promerica_alquimias, "predicciones_finales_promerica_alquimias.rds")
