# ---
# title: "Predicción de los montos de recuperación"
# author: "Georgina Ureña Ballestero"
# date: "2023-09-19"
# output: html_document
# ---
  
  
# ------------------------------ PREDICCIÓN DE LOS MONTOS DE RECUPERACIÓN ------------------------------ #
# ------------------------------ ESCENARIO 1: SERIE SIN COVID 19 ------------------------------ #
# ------------------------------ NIVEL PAÍS COSTA RICA ------------------------------ #
  
  
# --- ETAPA 1: PREPARACIÓN DE LA DATA Y DE LA SERIE DE TIEMPO
  
  
# Bibliotecas
library(dplyr)
library(DT)
library(dygraphs)
library(forecast)
library(fmsb)
library(fpp2)
library(ggplot2)
library(lubridate)
library(nortest)
library(padr) #When there are missing records for time points where observations were absent, pad will automatically insert these records
library(reshape)
library(tidyverse)
library(TSA)
library(xts)


# Carga del dataset

costa_rica_pagos = read.csv("C:/Users/gurenab/Documents/Predicción de recuperación/src/datasets/InformacionInst.csv",
                            header = FALSE,
                            sep = ";",
                            dec = ".") %>% as.data.frame()
names(costa_rica_pagos) = c("TIPO_CARTERA", "INSTITUCION", "ALQUIMIA", "PRODUCTO", "COMPROBANTE", "FECHA_PAGO", "NUMERO_OPERACION", "TAP_NOMBRE", "MONTO_RECIBO")


# Preparación de los datos

str(costa_rica_pagos)
costa_rica_pagos$FECHA_PAGO = as.Date(costa_rica_pagos$FECHA_PAGO,format="%d/%m/%Y")
summary(costa_rica_pagos)
# Se tienen observaciones desde 2000-03-19 hasta 2023-09-28 (no equiespaciadas o consecutivas)

# costa_rica_pagos = costa_rica_pagos %>%
#   arrange(MONTO)

# El análisis sólo se debe realizar para el tipo de cartera Propia, por lo cual se aplica este filtro. Adicionalmente, por indicaciones de Don Willie, se deben eliminar montos menores a 5000. De esta manera se agrupa el total de pagos por fecha, ya que el objetivo es pronosticar de forma global (en este caso) y no por cliente. 
cr_pagos = costa_rica_pagos %>% 
  filter(TIPO_CARTERA == "Propia") %>%
  filter(MONTO_RECIBO >= 5000.000) %>%
  group_by(FECHA_PAGO = floor_date(FECHA_PAGO, 'month')) %>%
  summarize(MONTO_TOTAL = sum(MONTO_RECIBO)) %>%
  # Add missing dates per month
  pad() %>%
  as.data.frame()

# El primer registro es 2000-03-01 con un monto total de 21700 y luego se vuelve a tener registro hasta 2001-10-01 con un monto total de 639000.0. La pandemia del Covid 19 empezó a inicios del 2020. Ante esto, decido empezar con el análisis a partir del 2019-01-01 para tener observaciones como si esa situación no hubiera existido y empezar la serie desde ese punto. Además, decido dejar como última observación el mes anterior completo (en este caso es agosto del 2023 porque la fecha actual es mediados de setiembre y no se tiene la totalidad del monto para este mes).  
currentDate = Sys.Date()
end_last_month <- rollback(currentDate)
init_last_month = rollback(end_last_month, roll_to_first = TRUE)

cr_pagos = cr_pagos %>%
  filter(FECHA_PAGO >= '2019-01-01' & FECHA_PAGO <= init_last_month)


# Pruebas de normalidad

# Se verifica la normalidad de las series
residuos_data = diff(cr_pagos$MONTO_TOTAL)

# Gráfico de los residuos
plot(residuos_data, type = "l")
# El gráfico muestra atipicidades?

# Histograma de los residuos
hist(residuos_data, prob = T, col = "lightblue")
# El histograma anterior presenta la distribución de los residuos para las observaciones. Se observa una distribución medio simétrica, lo que podría indicar que tal vez incluye valores atípicos.

# Comparación con qqnorm y qqline
qqnorm(residuos_data)
qqline(residuos_data, col = "red")
# Para determinar que los residuos se distribuyen de acuerdo a la normal, los datos deben ajustarse a la línea recta de color rojo. Sin embargo, para confirmar los supuestos realizados con los gráficos anteriores, lo correcto es realizar pruebas formales de normalidad.

# Prueba de normalidad chi-cuadrado de Pearson
# Hipótesis nula (H0): La muestra proviene de una distribución normal. 
# Hipótesis alternativa (H1): La muestra no proviene de una distribución normal.
pearson.test(residuos_data)
# Según la prueba anterior, se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-value = 0.6821) > (alfa = 0.05)].

# Prueba de normalidad Lilliefors (Kolmogorov - Smirnov)
lillie.test(residuos_data)
# Según la prueba anterior, se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-values = 0.3361) > (alfa = 0.05)].

# Prueba de normalidad Cramer-von Mises
cvm.test(residuos_data)
# Según la prueba anterior, se rechaza la hipótesis nula: Existe suficiente evidencia estadística para no rechazar la hipótesis nula de que la muestra (los residuos de la serie) proviene de una distribución normal [(p-values = 0.1884) > (alfa = 0.05)].


# Serie de tiempo

# Se crea la serie de tiempo. El patrón estacional es mensual, por eso la frecuencia es 12
serie = ts(cr_pagos$MONTO_TOTAL, start = c(2019,1), frequency = 12)
str(serie)
tail(serie)
# El objeto creado anteriormente inicia en 706.141.883 como monto de pago por todos los clientes de Costa Rica y finaliza en 1.337.089.712 en agosto 2023. Es decir, es una sucesión ordenada de valores.

# Gráfico de series de tiempo
autoplot(serie, main = "Montos totales recuperados", xlab = "Año", ylab = "Monto recuperado")


# Estacionariedad

# La prueba Dickey-Fuller se usa para detectar estadísticamente la presencia de conducta tendencial estocástica en las series temporales de las variables.
# Hipótesis nula (H0: δ = 0): La serie de tiempo es no estacionaria o tiene tendencia estocástica. 
# Hipótesis alternativa (H1: δ ≠ 0): No existe raíz unitaria, entonces la serie es estacionaria.
# La ADF es un número negativo. Mientras más negativo sea el estadístico ADF, más fuerte es el rechazo de la hipótesis nula sobre la existencia de una raíz unitaria o no estacionariedad. 
# Dickey-Fuller Ampliado (Test ADF). La hipótesis nula en este test es que la serie no es estacionaria, luego si el valor resultante, p-value, es menor de 0.05 (ya que el p-value representa la probabilidad de la hipótesis tomada) indica que la serie es estacionaria con un nivel de confianza del 95% (en caso contrario no habría evidencia para rechazar la hipótesis de no estacionariedad).

# Cálculo del test de Dickey-Fuller Ampliado (Test ADF) para estacionariedad: adf.test(serie temporal) paquete tseries siempre elimina la tendencia de la serie evaluada
tseries::adf.test(serie)
# Se obtiene un valor de -2.6819 y un p-value de 0.2996. La hipótesis nula es aceptada y, por lo tanto, la serie es no estacionaria. In other words, it has some time-dependent structure and does not have constant variance over time.  


# Autocorrelaciones

# Gráfico de correlación ACF simple
acf(serie)
# La correlación simple en el retardo k se refiere a la autocorrelación entre los valores de las series que se encuentran a k intervalos de distancia. El eje x del gráfico indica el retardo en el que se calcula la autocorrelación y el eje y indica el valor de la correlación (entre -1 y 1). Por ejemplo, un trazo de unión en el retardo 1 de un gráfico de ACF indica que existe una fuerte correlación entre el valor de cada serie y el valor anterior, un trazo de unión en el retardo 2 indica que existe una fuerte correlación entre el valor de cada serie y el valor que aparece dos puntos anteriores, etc. En este caso, las correlaciones positivas indican que los valores grandes actuales corresponden con valores grandes en el retardo especificado (si fuera una correlación negativa indicaría que los valores grandes actuales se corresponden con valores pequeños en el retardo especificado). El valor absoluto de una correlación es una medida de la fuerza de la asociación, con valores absolutos mayores que indican relaciones más fuertes. Las líneas discontinuas representan las bandas de confianza de ρ(k) de nivel 95% bajo la hipótesis de que la serie es un ruido blanco (incorrelada). En el caso anterior se puede apreciar en la gráfica como la serie denota, de alguna manera, que es estacionaria, ya que el valor de la autocorrelación decae paulatinamente. Sin embargo, no se aprecia que exista el componente estacional con un periodo claro.

# PACF partial autocorrelation graph
pacf(serie)
# La función de autocorrelación parcial, en el retardo k, es la autocorrelación entre los valores de las series que se encuentran a k intervalos de distancia, teniendo en cuenta los valores de los intervalos intermedios. El eje x del gráfico indica el retardo en el que se calcula la autocorrelación y el eje y indica el valor de la correlación (entre -1 y 1). Por ejemplo, un trazo de unión en el retardo 1 de un gráfico de PACF indica que existe una fuerte correlación entre el valor de cada serie y los valores de los intervalos intermedios anteriores, un trazo de unión en el retardo 2 indica que existe una fuerte correlación entre el valor de cada serie y los valores de los intervalos intermedios anteriores que aparecen dos puntos anteriores, etc. En este caso, las correlaciones positivas indican que los valores grandes actuales corresponden con valores grandes en el retardo especificado (si fuera una correlación negativa indicaría que los valores grandes actuales corresponden con valores pequeños en el retardo especificado). El valor absoluto de una correlación es una medida de la fuerza de la asociación, con valores absolutos mayores que indican relaciones más fuertes. Se puede apreciar en la gráfica como la serie denota, de alguna manera, que es estacionaria, ya que el valor de la autocorrelación decae (más abruptamente en ciertos trozos) a medida que aumentan los rezagos en el tiempo.
# Podemos apreciar que la serie sigue siendo estacionaria, ya que la función de autocorrelación no decrece rápidamente, sino de manera abrupta, en los desfaces regulares de forma lenta en los retardos, de forma que es estacionaria en la parte estacional.


# Descomposicion de la serie de tiempo
autoplot(stl(serie, s.window = "periodic"), ts.colour="blue")
# Es frecuente analizar las series temporales desde el punto de vista de sus componentes estructurales: La serie observada es la suma de la tendencia + el efecto estacional + los residuos. Es así como la serie observada es el resultado de sumar una tendencia que representa el comportamiento a largo plazo de la serie (tendencia a aumentar de manera paulatina, aunque al final de la serie parece decrecer), un efecto estacional que describe sus fluctuaciones periódicas (un claro comportamiento que se repite) y un componente residual que describe las variaciones a corto plazo, normalmente impredecibles.


# Períodos más importantes

# El espectro y la frecuencia son calculados
res = spec.pgram(serie, log = "no", plot = F)

# El espectro es ordenado de acuerdo a la frecuencia en orden decreciente (del más alto al más bajo)
pos = order(res$spec, res$freq, decreasing = TRUE)

# Se toman los 3 primeros períodos más importantes (No se toma la primera posición porque es la serie completa)
best3 = pos[pos != 1][1:3] # La posición 1 se ignora y se toman las primeras posiciones
best3
# 6 5 3

# Frecuencias de las posiciones obtenidas
frequencies = res$freq[best3]
frequencies
# 1.2 1.0 0.6

# Se obtienen los primeros períodos más importantes. El numerador es 12 porque la frecuencia definida para la serie es 12
periods = 12/frequencies
periods
# 10 12 20
# Es así como de determina que los períodos más importantes son cada 10 meses (poco menos del año), cada 12 meses (cada años) y cada 20 meses (poco menos de 2 años).

# Periodograma con el paquete TSA
res = periodogram(serie)
# El periodograma de una serie muestra la energía o varianza para cada una de las frecuencias. En el eje horizontal se detalla las frecuencias en el que se ha descompuesto la serie, mientras que en el eje vertical se encuentra el peso relativo o importancia de cada frecuencia; cabe destacar que el valor del eje vertical mostrado en el periodograma es la suma de los cuadrados de los pesos (seno y coseno) para esa frecuencia. Para su interpretación resulta útil compararlo con un sintonizador de radio: En él se observa la serie como la señal emitida, por lo cual el periodograma es el dial que busca en que frecuencia se oye mejor. Es así como se ve que esto ocurre en las primeras frecuencias.



# --- ETAPA 2: TRAINING Y TESTING, MODELOS Y PREDICCIONES


# Training y testing

# Decido utilizar para testing el 30% de la data; esto es de Marzo 2022 a lo que llevamos del 2023
proporcion_training = 0.70 #Para que la serie de training llegue hasta Marzo 2022
indices_training = length(serie)*proporcion_training

# Se crea la tabla para training con datos desde Enero del 2019 hasta Marzo 2022
serie_training = head(serie, indices_training)
head(serie_training)
tail(serie_training)

# Se crea la tabla de testing con datos de Abril 2022 a Agosto 2023
serie_testing = tail(serie, -indices_training)
head(serie_testing)
tail(serie_testing)


# ----------- MODELOS Y PREDICCIONES ----------- #

# Auto ARIMA

# Modelo Auto Arima
model_auto_arima = auto.arima(serie_training)
model_auto_arima

# Modelo Arima con los parámetros sugeridos por el Auto Arima
model_arima = arima(serie_training, order = c(0,1,1), seasonal = list(order = c(0,0,0), period = 12))
model_arima

# Predicciones con ARIMA
pred_arima = predict(model_arima, n.ahead = 17)
pred_arima

# Arima calibrado
calibrar_arima <- function(entrenamiento = NULL, prueba = NULL, periodo = NA_integer_, 
                           ar = 0:2, es = 0:1) {
  # todas las combinaciones son calculadas para los parámetros
  params <- purrr::cross(list(a = ar, b = ar, c = ar, d = es, e = es, f = es))
  
  # un modelo es calculado para cada combinación de parámetros
  arima_secure <- purrr::possibly(stats::arima, otherwise = NULL)
  models <- purrr::map(params, ~suppressWarnings(arima_secure(entrenamiento, order = c(.$a, 
                                                                                       .$b, .$c), seasonal = list(order = c(.$d, .$e, .$f), period = periodo))))
  
  # la predicción es calculada con cada modelo
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
  best_model$call <- call("arima", x = quote(datos), order = as.numeric(c(p$a, 
                                                                          p$b, p$c)), seasonal = list(order = as.numeric(c(p$d, p$e, p$f)), period = periodo))
  return(best_model)
}

# ARIMA es calibrado con la función anterior con escaneos hasta 5 para los parámetros D y Q (D = 5 y Q = 5, entre más altos sean los valores, más puede tardar el proceso)
calibrar_arima(serie_training, periodo = 12, 0:5, 0:5)

# Arima calibrado con los parámetros sugeridos 
model_calibrated_arima = arima(serie_training, order = c(1, 3, 0), seasonal = list(order = c(0, 1, 0), period = 12))
model_calibrated_arima

# Predicciones con Arima calibrado
pred_calibrated_arima = predict(model_calibrated_arima, n.ahead = 17)
pred_calibrated_arima

# Holt - Winters

# Modelos Holt-Winters
model_HW = HoltWinters(serie_training)
model_HW

# Predicciones con Holt-Winters
pred_HW = predict(model_HW, n.ahead = 17)
pred_HW

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
model_calibrated_HW = HoltWinters(x = serie_training, alpha = 0.3, beta = FALSE, gamma = 0.9)
model_calibrated_HW

# Predicciones con Holt-Winters calibrado
pred_calibrated_HW = predict(model_calibrated_HW, n.ahead = 17)
pred_calibrated_HW

# Todas las predicciones con autoplot
autoplot(serie_training, xlab = "Fecha", ylab = "", main = "Montos totales de recuperación para Costa Rica") + 
  autolayer(serie_testing, series = "Original") + 
  autolayer(pred_arima$pre, series = "ARIMA") + 
  autolayer(pred_calibrated_arima$pred, series = "ARIMA calibrado") + 
  autolayer(pred_HW, series = "Holt-Winters") + 
  autolayer(pred_calibrated_HW, series = "Holt-Winters calibrado") + 
  guides(colour = guide_legend(title = "Predicciones"))

# Errores

# RSS - Residual sum of squares (Error estándar de los residuos)
RSS <- function(Pred, Real) {
  return(sum((Real - Pred)^2))
}

# MSE - Mean squared error (Error cuadrático medio): Calcula el promedio de los errores elevados al cuadrado
MSE <- function(Pred, Real) {
  N <- length(Real)
  rss <- sum((Real - Pred)^2)
  return((1/N) * rss)
}

# RMSE - Root mean squared error (Raíz del error cuadrático medio)
RMSE <- function(Pred, Real) {
  N <- length(Real)
  rss <- sum((Real - Pred)^2)
  return(sqrt((1/N) * rss))
}

# PFA - Porcentaje de veces en las que el pronóstico fue mayor o igual a la realidad
PFA <- function(Pred, Real) {
  Total <- 0
  N <- length(Pred)
  for(i in 1:N) {
    if(Pred[i] > Real[i])
      Total <- Total + 1      
  }
  return(Total/N)
}

# PTFA - Porcentaje de fallos hacia arriba en términos absolutos. Es decir, donde el pronóstico fue mayor o igual a la realidad
PTFA <- function(Pred, Real) {
  Total <- 0
  SReal <- 0
  N <- length(Pred)
  for(i in 1:N) {
    if(Pred[i] > Real[i]) {
      Total <- Total + (Pred[i] - Real[i])
      SReal <- SReal + abs(Real[i])
    }
  }
  if(Total == 0)
    SReal = 1
  return(Total/SReal)
}

# Se obtienen todos los errores. Todas las predicciones en una lista y el valor real deben pasar como parámetros
tabla.errores <- function(predicciones, real, nombres = NULL) {
  r <- data.frame()
  for (pred in predicciones) {
    r <- rbind(r, data.frame(
      'MSE' = MSE(pred, real), 'RMSE' = RMSE(pred, real),
      'PFA' = PFA(pred, real), 'PTFA' = PTFA(pred, real)
    )
    )
  }
  row.names(r) <- nombres
  return(r)
}

errors <- tabla.errores(
  predicciones = list(pred_arima$pre, pred_calibrated_arima$pred, pred_HW, pred_calibrated_HW), 
  real = serie,
  nombres = c("ARIMA", "ARIMA calibrado", "Holt-Winters", "Holt-Winters calibrado")
)

errors
# El cuadro anterior detalla los índices de error para cada uno de los modelos planteados. De esta manera se confirma que Arima calibrado no es la mejor opción, ya que sus valores en MSE y RMSE son muy altos, mientras que Holt-Winters calibrado muestra lo contrario.

# Se grafican los errores. La función recibe un data frame con todos los errores a graficar (la tabla de datos obtenida con la función previa)
grafico.errores <- function (errors) {
  
  centros <- as.data.frame(apply(errors, 2, function(i)
    scales::rescale(i, to = c(0, 100))))
  
  res <- melt(t(centros), varnames = c("E", "Modelos"))
  res <- res[order(res$E, decreasing = F), ]
  res$M <- as.character(res$M)
  y = c(0, 25, 50, 75, 100)
  
  ggplot(res, aes(x = E, y = value, group = Modelos, color = Modelos, fill = Modelos)) +
    geom_polygon(alpha = 0.3, size = 1) + geom_point(size = 3) + 
    theme_minimal() + theme(axis.text.y = element_blank()) + xlab("") + 
    ylab("") + scale_y_continuous(limits = c(-10, 100), breaks = y) + 
    annotate("text", x = 0.5, y = y, label = paste0(y, "%"), color = "gray60") +
    ggproto("CordRadar", CoordPolar, theta = "x", r = "y", 
            start = 0, direction = sign(1))
}

grafico.errores(errors)

# Aunque se confirma, nuevamente, que el mejor modelo es Holt-Winters calibrado porque muestra valores más bajos para MSE y RMSE y PTFA, cabe destacar que se asemeja mucho al modelo Holt-Winters.



# --- ETAPA 3: MODELO DE PREDICCIÓN SELECCIONADO


# Mejor modelo
# Mejor modelo: Holt-Winters calibrado Se utiliza forecast para los límites de confianza.

# Modelo Holt-Winters calibrado
final_model = HoltWinters(x = serie, alpha = 0.3, beta = FALSE, gamma = 0.9)
final_model

# Predicciones con Holt-Winters calibrado
final_pred = forecast(final_model, h = 7, level = c(95))
final_pred

# De esta manera se muestran las predicciones para los últimos meses de este 2023 y el primer trimestre del 2024, así como sus respectivos intervalos con 95% de confianza.

# Unión de las predicciones
p = ts.union(prediction = final_pred$mean, LowerConfidenceLimit = final_pred$lower, UpperConfidenceLimit = final_pred$upper)

# Todas las predicciones
final_predictions = ts.union(serie, p)
final_predictions

# Fecha final para la serie original
final_date = cr_pagos$FECHA_PAGO[nrow(cr_pagos)]
final_date

# Nuevas fechas para las predicciones
dates = final_date + months(1:7)
dates

# Unión de las nuevas fechas con las fechas de la serie original
total_dates = c(cr_pagos$FECHA_PAGO, dates)

# Predicciones finales con dygraph
predictions = xts(xts(final_predictions, order.by = total_dates))

dygraph(predictions, width = "100%", ylab = "Monto", main = "Monto de recuperación para Costa Rica") %>% 
  dySeries(c("p.LowerConfidenceLimit", "p.prediction", "p.UpperConfidenceLimit"), label = "Prediction") %>% 
  dyRangeSelector(height = 20, strokeColor = "Gray")
# Lo anterior es la representación gráfica de las predicciones generadas y sus intervalos de confianza correspondientes.

# Predicciones finales como data.frame
fechas_finales = as.data.frame(total_dates)
predicciones_finales =  as.data.frame(predictions)

predicciones_finales_cr_escenario2 = cbind(fechas_finales,predicciones_finales) %>%
  as.data.frame()
names(predicciones_finales_cr_escenario2) = c("Fecha", "Monto_real", "Estimacion_puntual", "Limite_confianza_inferior", "Limite_confianza_superior")

# Guardar los resultados en formato RDS
saveRDS(predicciones_finales_cr_escenario2, "predicciones_finales_cr_escenario2.rds")
