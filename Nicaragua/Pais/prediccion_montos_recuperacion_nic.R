# ---
# title: "Predicción de los montos de recuperación"
# author: "Georgina Ureña Ballestero"
# date: "2023-12-05"
# output: html_document
# ---
  
  
# --- PREDICCIÓN DE LOS MONTOS DE RECUPERACIÓN --- #
# --- SERIE COMPLETA --- #
# --- NIVEL PAÍS NICARAGUA --- #

  
  
# --- ETAPA 1: PREPARACIÓN DE LA DATA Y DE LA SERIE DE TIEMPO
  
  
# Bibliotecas

library(DBI)
library(dplyr)
library(DT)
library(dygraphs)
library(forecast)
library(fmsb)
library(fpp2)
library(ggplot2)
library(lubridate)
library(nortest)
library(odbc)
library(padr) #When there are missing records for time points where observations were absent, pad will automatically insert these records
library(reshape)
library(tidyverse)
library(TSA)
library(xts)


# Conexión con SQL Server para carga de los datasets 

# Se define la conexión
connection <- dbConnect(odbc(),
                        Driver = "SQL Server",
                        Server = "200.74.250.96",
                        Database = "XPERSOFT_BD",
                        UID = "gurena",
                        PWD = "CC#E!rMzuWz3gjw@")

# Obtengo los datasets con las variables de interés y los criterios que debe cumplir.
nicaragua_pagos = dbGetQuery(connection, "
                              SELECT TIPO_CARTERA AS TIPO_CARTERA, INSTITUCION AS INSTITUCION, ALQUIMIA AS ALQUIMIA, 
                                FECHA_PAGO AS FECHA_PAGO, MONTO_RECIBO AS MONTO_RECIBO
                              FROM CRED_PAGOS
                              WHERE PAI_NOMBRE = 'NICARAGUA'
                                AND TIPO_CARTERA = 'Propia'
                                AND INSTITUCION!= 'Pagos sin identificar'
                                ")

# Se cierra la conexión
dbDisconnect(connection)


# Preparación de los datos

# str(nicaragua_pagos)
nicaragua_pagos$FECHA_PAGO = as.Date(nicaragua_pagos$FECHA_PAGO,format="%d/%m/%Y")
# summary(nicaragua_pagos)
# # Se tienen observaciones desde 2014-09 a hoy (no equiespaciadas o consecutivas)

# nicaragua_pagos = nicaragua_pagos %>%
#   arrange(MONTO)

# Se agrupa el total de pagos por fecha, ya que el objetivo es pronosticar de forma global (en este caso). 
nic_pagos = nicaragua_pagos %>% 
  group_by(FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO)) %>%
  # Add missing dates per month
  pad() %>%
  as.data.frame()

# Determinación del porcentaje de valores vacíos respecto del total de datos
# para cada columna
# porcentaje_empty = function(x) {sum(is.na(x)) / length(x)*100}
# por columna
# apply(nic_pagos, 2, porcentaje_empty)
# 0.0% de la data en la que no hubo recuperación.
# # Se podría trabajar desde la primera fecha de registro hasta la fecha actual.

# Se decide, entonces, empezar la serie en la primera observación y dejar como última observación el mes anterior al actual completo.  
init_first_month = nic_pagos$FECHA_PAGO[1] 

init_first_month_month = month(init_first_month)
init_first_month_year = year(init_first_month)

currentDate = Sys.Date()
end_last_month = rollback(currentDate)
init_last_month = rollback(end_last_month, roll_to_first = TRUE)

nic_pagos = nic_pagos %>%
  filter(FECHA_PAGO >= init_first_month & FECHA_PAGO <= init_last_month)

# # Pruebas de normalidad
# 
# # Se verifica la normalidad de las series
# residuos_data = diff(nic_pagos$MONTO_TOTAL)
# 
# # Gráfico de los residuos
# plot(residuos_data, type = "l")
# # El gráfico muestra atipicidades?
# 
# # Histograma de los residuos
# hist(residuos_data, prob = T, col = "lightblue")
# # El histograma anterior presenta la distribución de los residuos para las observaciones. Se observa una distribución relativamente simétrica, lo que podría indicar que tal vez no incluye valores atípicos.
# 
# # Comparación con qqnorm y qqline
# qqnorm(residuos_data)
# qqline(residuos_data, col = "red")
# # Para determinar que los residuos se distribuyen de acuerdo a la normal, los datos deben ajustarse a la línea recta de color rojo. Sin embargo, para confirmar los supuestos realizados con los gráficos anteriores, lo correcto es realizar pruebas formales de normalidad.
# 
# # Prueba de normalidad chi-cuadrado de Pearson
# # Hipótesis nula (H0): La muestra proviene de una distribución normal. 
# # Hipótesis alternativa (H1): La muestra no proviene de una distribución normal.
# pearson.test(residuos_data)
# # Según la prueba anterior, no se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-value = 0.4509) > (alfa = 0.05)].
# 
# # Prueba de normalidad Lilliefors (Kolmogorov - Smirnov)
# lillie.test(residuos_data)
# # Según la prueba anterior, no se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-values = 0.187) > (alfa = 0.05)].
# 
# # Prueba de normalidad Cramer-von Mises
# cvm.test(residuos_data)
# # Según la prueba anterior, no se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-values = 0.5557) > (alfa = 0.05)].


# Serie de tiempo

# Se crea la serie de tiempo. El patrón estacional es mensual, por eso la frecuencia es 12
serie = ts(nic_pagos$MONTO_TOTAL, start = c(init_first_month_year,init_first_month_month), frequency = 12)
# str(serie)
# head(serie)
# tail(serie)
# El objeto creado anteriormente inicia en 1663158 como monto de pago por todos los clientes de Nicaragua y finaliza en 1883725 en noviembre 2023. Es decir, es una sucesión ordenada de valores.

# # Gráfico de series de tiempo
# autoplot(serie, main = "Montos totales recuperados", xlab = "Anno", ylab = "Monto recuperado")


# Estacionariedad

# La prueba Dickey-Fuller se usa para detectar estadísticamente la presencia de conducta tendencial estocástica en las series temporales de las variables.
# Hipótesis nula (H0: δ = 0): La serie de tiempo es no estacionaria o tiene tendencia estocástica. 
# Hipótesis alternativa (H1: δ ≠ 0): No existe raíz unitaria, entonces la serie es estacionaria.
# La ADF es un número negativo. Mientras más negativo sea el estadístico ADF, más fuerte es el rechazo de la hipótesis nula sobre la existencia de una raíz unitaria o no estacionariedad. 
# Dickey-Fuller Ampliado (Test ADF). La hipótesis nula en este test es que la serie no es estacionaria, luego si el valor resultante, p-value, es menor de 0.05 (ya que el p-value representa la probabilidad de la hipótesis tomada) indica que la serie es estacionaria con un nivel de confianza del 95% (en caso contrario no habría evidencia para rechazar la hipótesis de no estacionariedad).

# # Cálculo del test de Dickey-Fuller Ampliado (Test ADF) para estacionariedad: adf.test(serie temporal) paquete tseries siempre elimina la tendencia de la serie evaluada
# tseries::adf.test(serie)
# # Se obtiene un valor de -3.4643 y un p-value de 0.0486. La hipótesis nula no es aceptada y, por lo tanto, la serie es estacionaria. In other words, it has some time-dependent structure and does not have constant variance over time.  


# # Autocorrelaciones
# 
# # Gráfico de correlación ACF simple
# acf(serie)
# # La correlación simple en el retardo k se refiere a la autocorrelación entre los valores de las series que se encuentran a k intervalos de distancia. El eje x del gráfico indica el retardo en el que se calcula la autocorrelación y el eje y indica el valor de la correlación (entre -1 y 1). Por ejemplo, un trazo de unión en el retardo 1 de un gráfico de ACF indica que existe una fuerte correlación entre el valor de cada serie y el valor anterior, un trazo de unión en el retardo 2 indica que existe una fuerte correlación entre el valor de cada serie y el valor que aparece dos puntos anteriores, etc. En este caso, las correlaciones positivas indican que los valores grandes actuales corresponden con valores grandes en el retardo especificado (si fuera una correlación negativa indicaría que los valores grandes actuales se corresponden con valores pequeños en el retardo especificado). El valor absoluto de una correlación es una medida de la fuerza de la asociación, con valores absolutos mayores que indican relaciones más fuertes. Las líneas discontinuas representan las bandas de confianza de ρ(k) de nivel 95% bajo la hipótesis de que la serie es un ruido blanco (incorrelada). En el caso anterior se puede apreciar en la gráfica como la serie denota, de alguna manera, que es estacionaria, ya que el valor de la autocorrelación decae paulatinamente. Sin embargo, no se aprecia que exista el componente estacional con un periodo claro.
# 
# # PACF partial autocorrelation graph
# pacf(serie)
# # La función de autocorrelación parcial, en el retardo k, es la autocorrelación entre los valores de las series que se encuentran a k intervalos de distancia, teniendo en cuenta los valores de los intervalos intermedios. El eje x del gráfico indica el retardo en el que se calcula la autocorrelación y el eje y indica el valor de la correlación (entre -1 y 1). Por ejemplo, un trazo de unión en el retardo 1 de un gráfico de PACF indica que existe una fuerte correlación entre el valor de cada serie y los valores de los intervalos intermedios anteriores, un trazo de unión en el retardo 2 indica que existe una fuerte correlación entre el valor de cada serie y los valores de los intervalos intermedios anteriores que aparecen dos puntos anteriores, etc. En este caso, las correlaciones positivas indican que los valores grandes actuales corresponden con valores grandes en el retardo especificado (si fuera una correlación negativa indicaría que los valores grandes actuales corresponden con valores pequeños en el retardo especificado). El valor absoluto de una correlación es una medida de la fuerza de la asociación, con valores absolutos mayores que indican relaciones más fuertes. Se puede apreciar en la gráfica como la serie denota, de alguna manera, que es estacionaria, ya que el valor de la autocorrelación decae (más abruptamente en ciertos trozos) a medida que aumentan los rezagos en el tiempo.
# # Podemos apreciar que la serie sigue siendo estacionaria, ya que la función de autocorrelación no decrece rápidamente, sino de manera abrupta, en los desfaces regulares de forma lenta en los retardos, de forma que es estacionaria en la parte estacional.


# # Descomposicion de la serie de tiempo
# 
# autoplot(stl(serie, s.window = "periodic"), ts.colour="blue")
# # Es frecuente analizar las series temporales desde el punto de vista de sus componentes estructurales: La serie observada es la suma de la tendencia + el efecto estacional + los residuos. Es así como la serie observada es el resultado de sumar una tendencia que representa el comportamiento a largo plazo de la serie (tendencia a aumentar de manera paulatina, aunque al final de la serie parece decrecer), un efecto estacional que describe sus fluctuaciones periódicas (un claro comportamiento que se repite) y un componente residual que describe las variaciones a corto plazo, normalmente impredecibles.
# 
# # Períodos más importantes
# 
# # El espectro y la frecuencia son calculados
# res = spec.pgram(serie, log = "no", plot = F)
# 
# # El espectro es ordenado de acuerdo a la frecuencia en orden decreciente (del más alto al más bajo)
# pos = order(res$spec, res$freq, decreasing = TRUE)
# 
# # Se toman los 3 primeros períodos más importantes (No se toma la primera posición porque es la serie completa)
# best3 = pos[pos != 1][1:3] # La posición 1 se ignora y se toman las primeras posiciones
# best3
# # 2  3 10
# 
# # Frecuencias de las posiciones obtenidas
# frequencies = res$freq[best3]
# frequencies
# # 0.2 0.3 1.0
# 
# # Se obtienen los primeros períodos más importantes. El numerador es 12 porque la frecuencia definida para la serie es 12
# periods = 12/frequencies
# periods
# # 60 40 12
# # Es así como de determina que los períodos más importantes son cada 60 meses (cada 5 años), cada 40 meses (cada poco más de 3 años) y cada 135.0 meses (poco más de cada 11 años).
# 
# # Periodograma con el paquete TSA
# res = periodogram(serie)
# # El periodograma de una serie muestra la energía o varianza para cada una de las frecuencias. En el eje horizontal se detalla las frecuencias en el que se ha descompuesto la serie, mientras que en el eje vertical se encuentra el peso relativo o importancia de cada frecuencia; cabe destacar que el valor del eje vertical mostrado en el periodograma es la suma de los cuadrados de los pesos (seno y coseno) para esa frecuencia. Para su interpretación resulta útil compararlo con un sintonizador de radio: En él se observa la serie como la señal emitida, por lo cual el periodograma es el dial que busca en que frecuencia se oye mejor. Es así como se ve que esto ocurre en las primeras frecuencias.



# --- ETAPA 2: TRAINING Y TESTING, MODELOS Y PREDICCIONES


# Training y testing

# Decido utilizar para testing el 20% de la data
proporcion_training = 0.80
indices_training = length(serie)*proporcion_training

# Se crea la tabla para training
serie_training = head(serie, indices_training)
# head(serie_training)
# tail(serie_training)

# Se crea la tabla de testing
serie_testing = tail(serie, -indices_training)
# head(serie_testing)
# tail(serie_testing)
n_testing = length(serie_testing)


# ----------- MODELOS Y PREDICCIONES ----------- #

# # Auto ARIMA
# 
# # Modelo Auto Arima
# model_auto_arima = auto.arima(serie_training)
# model_auto_arima
# 
# 
# # Modelo Arima con los parámetros sugeridos por el Auto Arima
# model_arima = arima(serie_training, order = c(2,1,2), seasonal = list(order = c(0,0,0), period = 12))
# model_arima
# 
# 
# # Predicciones con ARIMA
# pred_arima = predict(model_arima, n.ahead = n_testing)
# pred_arima
# 
# # Arima calibrado
# calibrar_arima <- function(entrenamiento = NULL, prueba = NULL, periodo = NA_integer_, 
#                            ar = 0:2, es = 0:1) {
#   # todas las combinaciones son calculadas para los parámetros
#   params <- purrr::cross(list(a = ar, b = ar, c = ar, d = es, e = es, f = es))
#   
#   # un modelo es calculado para cada combinación de parámetros
#   arima_secure <- purrr::possibly(stats::arima, otherwise = NULL)
#   models <- purrr::map(params, ~suppressWarnings(arima_secure(entrenamiento, order = c(.$a, 
#                                                                                        .$b, .$c), seasonal = list(order = c(.$d, .$e, .$f), period = periodo))))
#   
#   # la predicción es calculada con cada modelo
#   predictions <- purrr::map(models, ~{
#     if (is.null(.)) {
#       return(NULL)
#     }
#     forecast(., h = length(prueba))
#   })
#   
#   # el error es calculado para cada predicción
#   error <- purrr::map_dbl(predictions, ~{
#     if (is.null(.)) {
#       return(Inf)
#     }
#     sum((as.numeric(prueba) - as.numeric(.$mean))^2)
#   })
#   
#   # se retorna el modelo con el error más bajo
#   best_model <- models[[which.min(error)]]
#   p <- params[[which.min(error)]]
#   best_model$call <- call("arima", x = quote(datos), order = as.numeric(c(p$a, 
#                                                                           p$b, p$c)), seasonal = list(order = as.numeric(c(p$d, p$e, p$f)), period = periodo))
#   return(best_model)
# }
# 
# 
# # ARIMA es calibrado con la función anterior con escaneos hasta 5 para los parámetros D y Q (D = 5 y Q = 5, entre más altos sean los valores, más puede tardar el proceso)
# calibrar_arima(serie_training, periodo = 12, 0:5, 0:5)
# 
# # Arima calibrado con los parámetros sugeridos 
# model_calibrated_arima = arima(serie_training, order = c(0, 2, 0), seasonal = list(order = c(1, 1, 1), period = 12))
# model_calibrated_arima
# 
# # Predicciones con Arima calibrado
# pred_calibrated_arima = predict(model_calibrated_arima, n.ahead = n_testing)
# pred_calibrated_arima
# 
# # Holt - Winters
# 
# # Modelos Holt-Winters
# model_HW = HoltWinters(serie_training)
# model_HW
# 
# # Predicciones con Holt-Winters
# pred_HW = predict(model_HW, n.ahead = n_testing)
# pred_HW

# Función para calibrar Holt-Winters
calibrar_HW <- function(entrenamiento, prueba, paso = 0.1) {
  # todas las combinaciones son calculadas para los parámetros
  params <- purrr::cross(list(a = seq(0, 1, by = paso), b = seq(0, 1, by = paso),  
                              g = seq(0, 1, by = paso)))
  
  # un modelo es calculado para cada combinación de parámetros
  hw_secure <- purrr::possibly(stats::HoltWinters, otherwise = NULL)
  models <- purrr::map(params, ~suppressWarnings(hw_secure(entrenamiento, alpha = ifelse(.$a == 
                                                                                           0, F, .$a), beta = ifelse(.$b == 0, F, .$b), gamma = ifelse(.$g == 0, F, 
                                                                                                                                                       .$g))))
  
  # la predicción es calculada para cada modelo
  predictions <- purrr::map(models, ~{
    if (is.null(.)) {
      return(NULL)
    }
    forecast(., h = length(prueba))
  })
  
  # el error es calculado para cada predicción
  error <- purrr::map_dbl(predictions, ~{
    if (is.null(.)) {
      return(Inf)
    }
    sum((as.numeric(prueba) - as.numeric(.$mean))^2)
  })
  
  # se retorna el modelo con el error más bajo
  best_model <- models[[which.min(error)]]
  p <- params[[which.min(error)]]
  best_model$call <- call("HoltWinters", x = quote(datos), alpha = ifelse(p$a == 
                                                                            0, F, p$a), beta = ifelse(p$b == 0, F, p$b), gamma = ifelse(p$g == 0, F, 
                                                                                                                                        p$g))
  return(best_model)
}

# Holt-Winters es calibrado con la función anterior
calibrar_HW(serie_training, serie_testing)

# Modelo Holt-Winters calibrado con los parámetros sugeridos
model_calibrated_HW = HoltWinters(x = serie_training, alpha = calibrar_HW(serie_training, serie_testing)$alpha, beta = calibrar_HW(serie_training, serie_testing)$beta, gamma = calibrar_HW(serie_training, serie_testing)$gamma)
model_calibrated_HW

# # Predicciones con Holt-Winters calibrado
# pred_calibrated_HW = predict(model_calibrated_HW, n.ahead = n_testing)
# pred_calibrated_HW

# # Todas las predicciones con autoplot
# autoplot(serie_training, xlab = "Fecha", ylab = "", main = "Montos totales de recuperacion para Nicaragua") +
#   autolayer(serie_testing, series = "Original") +
#   autolayer(pred_arima$pre, series = "ARIMA") +
#   autolayer(pred_calibrated_arima$pred, series = "ARIMA calibrado") +
#   autolayer(pred_HW, series = "Holt-Winters") +
#   autolayer(pred_calibrated_HW, series = "Holt-Winters calibrado") +
#   guides(colour = guide_legend(title = "Predicciones"))

# # Errores
# 
# # RSS - Residual sum of squares (Error estándar de los residuos)
# RSS <- function(Pred, Real) {
#   return(sum((Real - Pred)^2))
# }
# 
# # MSE - Mean squared error (Error cuadrático medio): Calcula el promedio de los errores elevados al cuadrado
# MSE <- function(Pred, Real) {
#   N <- length(Real)
#   rss <- sum((Real - Pred)^2)
#   return((1/N) * rss)
# }
# 
# # RMSE - Root mean squared error (Raíz del error cuadrático medio)
# RMSE <- function(Pred, Real) {
#   N <- length(Real)
#   rss <- sum((Real - Pred)^2)
#   return(sqrt((1/N) * rss))
# }
# 
# # PFA - Porcentaje de veces en las que el pronóstico fue mayor o igual a la realidad
# PFA <- function(Pred, Real) {
#   Total <- 0
#   N <- length(Pred)
#   for(i in 1:N) {
#     if(Pred[i] > Real[i])
#       Total <- Total + 1      
#   }
#   return(Total/N)
# }
# 
# # PTFA - Porcentaje de fallos hacia arriba en términos absolutos. Es decir, donde el pronóstico fue mayor o igual a la realidad
# PTFA <- function(Pred, Real) {
#   Total <- 0
#   SReal <- 0
#   N <- length(Pred)
#   for(i in 1:N) {
#     if(Pred[i] > Real[i]) {
#       Total <- Total + (Pred[i] - Real[i])
#       SReal <- SReal + abs(Real[i])
#     }
#   }
#   if(Total == 0)
#     SReal = 1
#   return(Total/SReal)
# }
# 
# # Se obtienen todos los errores. Todas las predicciones en una lista y el valor real deben pasar como parámetros
# tabla.errores <- function(predicciones, real, nombres = NULL) {
#   r <- data.frame()
#   for (pred in predicciones) {
#     r <- rbind(r, data.frame(
#       'MSE' = MSE(pred, real), 'RMSE' = RMSE(pred, real),
#       'PFA' = PFA(pred, real), 'PTFA' = PTFA(pred, real)
#     )
#     )
#   }
#   row.names(r) <- nombres
#   return(r)
# }
# 
# errors <- tabla.errores(
#   predicciones = list(pred_arima$pre, pred_calibrated_arima$pred, pred_HW, pred_calibrated_HW), 
#   real = serie,
#   nombres = c("ARIMA", "ARIMA calibrado", "Holt-Winters", "Holt-Winters calibrado")
# )
# 
# errors
# # El cuadro anterior detalla los índices de error para cada uno de los modelos planteados. De esta manera se confirma que Arima calibrado no es la mejor opción, ya que sus valores en MSE y RMSE son muy altos, mientras que Holt-Winters calibrado muestra lo contrario.
# 
# # Se grafican los errores. La función recibe un data frame con todos los errores a graficar (la tabla de datos obtenida con la función previa)
# grafico.errores <- function (errors) {
# 
#   centros <- as.data.frame(apply(errors, 2, function(i)
#     scales::rescale(i, to = c(0, 100))))
# 
#   res <- melt(t(centros), varnames = c("E", "Modelos"))
#   res <- res[order(res$E, decreasing = F), ]
#   res$M <- as.character(res$M)
#   y = c(0, 25, 50, 75, 100)
# 
#   ggplot(res, aes(x = E, y = value, group = Modelos, color = Modelos, fill = Modelos)) +
#     geom_polygon(alpha = 0.3, size = 1) + geom_point(size = 3) +
#     theme_minimal() + theme(axis.text.y = element_blank()) + xlab("") +
#     ylab("") + scale_y_continuous(limits = c(-10, 100), breaks = y) +
#     annotate("text", x = 0.5, y = y, label = paste0(y, "%"), color = "gray60") +
#     ggproto("CordRadar", CoordPolar, theta = "x", r = "y",
#             start = 0, direction = sign(1))
# }
# 
# grafico.errores(errors)
# 
# # Aunque se confirma, nuevamente, que el mejor modelo es Holt-Winters calibrado porque muestra valores más bajos para MSE y RMSE, cabe destacar que se asemeja mucho al modelo Holt-Winters y ARIMA.



# --- ETAPA 3: MODELO DE PREDICCIÓN SELECCIONADO


# Mejor modelo
# Mejor modelo: Holt-Winters calibrado Se utiliza forecast para los límites de confianza.

# Parámetros
model_calibrated_HW_alpha = model_calibrated_HW$alpha
model_calibrated_HW_beta = model_calibrated_HW$beta
model_calibrated_HW_gamma = model_calibrated_HW$gamma

# Modelo Holt-Winters calibrado
final_model = HoltWinters(x = serie, alpha = model_calibrated_HW_alpha, beta = model_calibrated_HW_beta, gamma = model_calibrated_HW_gamma)
final_model

# Predicciones con Holt-Winters calibrado
final_pred = forecast(final_model, h = 6, level = c(95))
final_pred

# De esta manera se muestran las predicciones para los últimos meses de este 2023 y el primer trimestre del 2024, así como sus respectivos intervalos con 95% de confianza.

# Unión de las predicciones
p = ts.union(prediction = final_pred$mean, LowerConfidenceLimit = final_pred$lower, UpperConfidenceLimit = final_pred$upper)

# Todas las predicciones
final_predictions = ts.union(serie, p)
final_predictions

# Fecha final para la serie original
final_date = nic_pagos$FECHA_PAGO[nrow(nic_pagos)]
final_date

# Nuevas fechas para las predicciones
dates = final_date + months(1:6)
dates

# Unión de las nuevas fechas con las fechas de la serie original
total_dates = c(nic_pagos$FECHA_PAGO, dates)

# Predicciones finales con dygraph
predictions = xts(xts(final_predictions, order.by = total_dates))

# dygraph(predictions, width = "100%", ylab = "Monto", main = "Monto de recuperacion para Nicaragua") %>%
#   dySeries(c("p.LowerConfidenceLimit", "p.prediction", "p.UpperConfidenceLimit"), label = "Prediction") %>%
#   dyRangeSelector(height = 20, strokeColor = "Gray") %>%
#   dyOptions(maxNumberWidth = 20) %>%
#   dyAxis("y", valueRange = c(0, 5000000))
# 
# # Lo anterior es la representación gráfica de las predicciones generadas y sus intervalos de confianza correspondientes.

# Predicciones finales como data.frame
fechas_finales = as.data.frame(total_dates)
predicciones_finales =  as.data.frame(predictions)

predicciones_finales_nic= cbind(fechas_finales,predicciones_finales) %>%
  as.data.frame()
names(predicciones_finales_nic) = c("Fecha", "Monto_real", "Estimacion_puntual", "Limite_confianza_inferior", "Limite_confianza_superior")
predicciones_finales_nic = predicciones_finales_nic %>%
  mutate(Limite_confianza_inferior = replace(Limite_confianza_inferior, Limite_confianza_inferior < 0, 0))

# Guardar los resultados en formato RDS
saveRDS(predicciones_finales_nic, "predicciones_finales_nic.rds")
