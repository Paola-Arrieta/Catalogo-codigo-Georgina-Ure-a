### TITLE: PERFIL DEL DEUDOR ###
### AUTHOR: GEORGINA UREÑA BALLESTERO ###


######################################
############# CLUSTERING #############
# Método clustering jerárquico mixto #
######################################

# Es importante recordar que se había llevo a cabo un análisis previo referente a la predicción de pago de los clientes. En él se había determinado como 
# filtro que se consideraran únicamente aquellos casos en los que la variable ESTADO de la tabla "juicios" se encontraran "en trámite".
# Además, se utilizó como criterio de definición para la variable AL_MENOS_UN_PAGO (variable respuesta en el contexto de predicciones de pago) el que en la 
# variable ULTIMO_PAGO > 1980/01/01; es decir, que se hubiera realizado el último pago de esa fecha en adelante. 
# Por otro lado, la EDAD se calculó a partir de la FECHA_NACIMIENTO al día de cálculo del análisis de predicción de pago.


# PASO 1: CARGAR DATOS EN FORMATOS REQUERIDOS

library(dplyr)

# Carga de la data
cartera_final = readRDS(file = "cartera_final.rds")
str(cartera_final)

# Formato correcto de la variable AL_MENOS_UN_PAGO
cartera_final$AL_MENOS_UN_PAGO = as.factor(cartera_final$AL_MENOS_UN_PAGO)
cartera_final = cartera_final %>%
  mutate(AL_MENOS_UN_PAGO = dplyr::recode(AL_MENOS_UN_PAGO,
                                          "0" = "no_pago",
                                          "1" = "si_pago"))

# Summary de la data
## skim() is an alternative to summary(), quickly providing a broad overview of a data frame. It handles data of all types, dispatching a different 
## set of summary functions based on the types of columns in the data frame.
skimr::skim(cartera_final) 

## Anteriormente se había realizado un PCA (en variables cuantitativas) para reducción de dimensiones; se utilizarán las variables seleccionadas. 
## Lo mismo con las variables cualitativas. Se había hecho un MCA para variables cualitativas, pero en este análisis se tomarán en cuenta todas excepto 
## REA_DESCRIPCION (ya que es exactamente la misma que REACCION_OPERACION), CANTIDAD_HIJOS_CATEGORIA (en el análisis anterior se determinó que la cantidad de
## hijos no es una de las variables que más contribuyen a la varianza explicada y ya se posee la cantidad de hijos numérica), ALQ_DESCRIPCION (debido a que
## tiene 251 niveles, lo que complica y casi que convierte inmanejable la interpretación de los resultados que se van obteniendo) y REACCION_OPERACION porque
## es muy similar a REACCION_ULTIMA_GESTION..
cartera_final = cartera_final %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION,
         MAO_SALDO, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO,
         CANTIDAD_PROMESAS_PAGO_CUMPLIDAS,INTERES, CANTIDAD_GESTIONES, EDAD, MONTO_ULTIMO_PAGO,
         EOP_DESCRIPCION, REACCION_ULTIMA_GESTION, TIENE_EXPEDIENTE, TIPO, SEXO, ESTADO_CIVIL, AL_MENOS_UN_PAGO)

table(cartera_final$AL_MENOS_UN_PAGO)
cartera_final_original = as.data.frame(cartera_final)


# PASO 2: ESCALAR LAS VARIABLES CUANTITATIVAS Y SUBMUESTREO

# Se recomienda normalizar o escalar los datos para que todas las variables tengan una influencia similar en el cálculo de la distancia. Sin embargo, en
# algunos métodos no es estrictamente necesario escalar los datos (como en el caso del clustering jerárquico), ya que el algoritmo utiliza directamente la
# distancia entre los puntos. Si las variables tienen escalas muy diferentes, puede ser útil escalarlas para mejorar la interpretación de los resultados y
# evitar que una variable domine sobre las demás en el cálculo de la distancia.

# Identificar la clase con menor frecuencia
minority_class = names(sort(table(cartera_final$AL_MENOS_UN_PAGO))[1])
# minority_class

# Submuestrear la clase mayoritaria
cartera_balanceada = cartera_final %>%
  group_by(AL_MENOS_UN_PAGO) %>%
  slice_sample(n = min(table(cartera_final$AL_MENOS_UN_PAGO)), replace = FALSE) %>%
  ungroup()

n_before = nrow(cartera_final)
n_after = nrow(cartera_balanceada)

cat("Número de observaciones antes del balanceo:", n_before, "\n")
cat("Número de observaciones después del balanceo:", n_after, "\n")

table(cartera_balanceada$AL_MENOS_UN_PAGO)  
cartera_balanceada_original = as.data.frame(cartera_balanceada)


# PASO 3: DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS
# Se calcularán las distancias por lotes debido a que el vector que se debe generar es muy grande para la capacidad computacional.

# Identificar columnas numéricas
columnas_numericas <- sapply(cartera_balanceada, is.numeric)

cartera_balanceada[, columnas_numericas] <- scale(cartera_balanceada[, columnas_numericas])

# Summary de la data
skimr::skim(cartera_balanceada) 
str(cartera_balanceada)

# Seleccionar variables que no son de tipo character
cartera_mixta <- select_if(cartera_balanceada, function(x) !is.character(x))

# Tamaño del lote
batch_size <- 1000 # Ajustar según sea necesario
num_batches <- ceiling(nrow(cartera_mixta) / batch_size)

# Inicializar una lista para almacenar las matrices de distancias de cada lote
dist_matrices <- list()

# Calcular la matriz de distancias para cada lote
for (i in 1:num_batches) {
  start_index <- (i - 1) * batch_size + 1
  end_index <- min(i * batch_size, nrow(cartera_mixta))
  batch_data <- cartera_mixta[start_index:end_index, ]
  # Calcular las distancias solo para el subconjunto actual de datos
  dist_matrices[[i]] <- cluster::daisy(batch_data)
}

# Combinar las matrices de distancias en una sola matriz
dist_matrix <- Reduce(`+`, dist_matrices)

# Calcular la suma de cuadrados intra-cluster para diferentes valores de k
wss <- sapply(2:10, function(k) {
  km <- kmeans(t(dist_matrix), centers = k)  # Transponer dist_matrix
  sum(km$withinss)
})

# Identificar el codo en la curva manualmente
elbow <- which(diff(wss) < 0)[1] + 1

# Graficar la curva de la suma de cuadrados intra-cluster en función del número de clusters
plot(2:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters",
     ylab = "Suma de cuadrados intra-cluster")
points(elbow, wss[elbow], col = "red", pch = 19)


# Convertir la matriz de distancias a una matriz de datos
data_matrix <- as.matrix(dist_matrix)

# Calcular el estadístico Gap nuevamente con la matriz de datos
# El estadístico Gap proporciona una estimación del número óptimo de clusters basándose en la comparación de la distribución de los datos con
# un conjunto de datos aleatorios. El número óptimo de clusters se determina encontrando el valor de k para el cual el estadístico Gap es
# máximo o alcanza un máximo relativo.
gap_stat <- cluster::clusGap(data_matrix, kmeans, K.max = 10, B = 50)

# Obtener el número óptimo de clusters
optimal_clusters <- gap_stat$Tab[which.max(gap_stat$Tab[, "gap"]), "logW"]
optimal_clusters = round(optimal_clusters,0)
optimal_clusters

# Convertir variables de tipo factor a numéricas
cartera_mixta_numeric <- model.matrix(~ . - 1, data = cartera_mixta)


# PASO 4: APLICAR CLUSTERING JERÁRQUICO MIXTO

# Aplicar K-means con el número óptimo de clusters
set.seed(123)  # Establecer una semilla para reproducibilidad
kmeans_model <- kmeans(cartera_mixta_numeric, centers = optimal_clusters)

# Obtener las asignaciones de cluster para cada observación
cluster_assignments <- kmeans_model$cluster

# Agregar las asignaciones de cluster al dataframe original
cartera_mixta_con_clusters <- cbind(cartera_mixta, Cluster = cluster_assignments)


# PASO 5: AGREGAR EL CLUSTER A LA DATA ORIGINAL (cartera_final_original - sin escalar)

# Agregar la variable Cluster a cartera_balanceada_original
cartera_balanceada_original$CLUSTER <- cartera_mixta_con_clusters$Cluster
table(cartera_balanceada_original$CLUSTER)


# # PASO 6: EVALUACIÓN DEL RENDIMIENTO DEL MODELO
# 
# library(clusterCrit)
#
# # Calcular el índice de silueta
# silhouette_scores <- cluster::silhouette(dist_matrix, cluster_codes)
#
# # Obtener el promedio del índice de silueta
# mean_silhouette <- mean(silhouette_scores[, "sil_width"])
# mean_silhouette
# 
# # Calcular el índice de Dunn
# library(fpc)
# dunn_index <- dunn(d = dist_matrix, cl = cluster_assignments)
# 
# # Calcular el índice de Dunn manualmente
# dunn_index <- function(dist_matrix, cluster_assignments) {
#   num_clusters <- max(cluster_assignments)
#   within_cluster_distances <- numeric(num_clusters)
#   
#   # Calcular las distancias dentro de los clusters
#   for (i in 1:num_clusters) {
#     cluster_points <- which(cluster_assignments == i)
#     if (length(cluster_points) > 1) {
#       cluster_dist <- dist_matrix[cluster_points, cluster_points]
#       within_cluster_distances[i] <- mean(cluster_dist[lower.tri(cluster_dist)])
#     } else {
#       within_cluster_distances[i] <- 0
#     }
#   }
#   
#   # Calcular las distancias entre los clusters
#   between_cluster_distances <- dist_matrix[lower.tri(dist_matrix)]
#   min_between_cluster_distance <- min(between_cluster_distances)
#   
#   # Calcular el índice de Dunn
#   dunn <- min_between_cluster_distance / max(within_cluster_distances)
#   return(dunn)
# }
# 
# # Calcular el índice de Dunn
# dunn_index <- dunn_index(dist_matrix, cluster_assignments)
# 
# # Calcular el índice Davies-Bouldin
# davies_bouldin_index <- davies_bouldin(cluster_assignments, dist_matrix)
# 
#
# PASO 7: DESCRIPCIÓN DE CADA CLUSTER

cartera_balanceada_original$CLUSTER <- as.factor(cartera_balanceada_original$CLUSTER)
str(cartera_balanceada_original)


# 1. Variables cuantis

# Lista de variables para las que quieres calcular métricas
variables_of_interest <- c("EDAD", "MAO_SALDO", "MAO_MONTO_ORIGINAL", "CANTIDAD_PROMESAS_PAGO",
                           "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS", "INTERES", "CANTIDAD_GESTIONES", "MONTO_ULTIMO_PAGO")

calculate_metrics <- function(data, variable_name) {
  # Calculate range within one standard deviation of the mean
  calculate_range <- function(values) {
    mean_val <- mean(values)
    sd_val <- sd(values)
    lower_limit <- mean_val - sd_val
    upper_limit <- mean_val + sd_val
    return(paste0("(", ifelse(lower_limit < 0, 0, round(lower_limit, 2)), ",", round(upper_limit, 2), ")"))
  }
  
  # Replace negative values with 0
  data[[variable_name]] <- ifelse(data[[variable_name]] < 0, 0, data[[variable_name]])
  
  # Calculate metrics for each cluster
  metrics_results <- data %>%
    group_by(CLUSTER) %>%
    summarise(
      variable = variable_name,
      n = n(),
      total = sum(.data[[variable_name]], na.rm = TRUE),
      mean = ifelse(is.numeric(.data[[variable_name]]), mean(.data[[variable_name]], na.rm = TRUE), NA),
      median = ifelse(is.numeric(.data[[variable_name]]), median(.data[[variable_name]], na.rm = TRUE), NA),
      sd = ifelse(is.numeric(.data[[variable_name]]), sd(.data[[variable_name]], na.rm = TRUE), NA),
      min = min(.data[[variable_name]], na.rm = TRUE),
      max = max(.data[[variable_name]], na.rm = TRUE),
      range_mean_sd = calculate_range(.data[[variable_name]]))
  
  return(metrics_results)
}

# Aplicar la función calculate_metrics a cada variable de interés
results_list <- lapply(variables_of_interest, function(variable) {
  calculate_metrics(cartera_balanceada_original, variable)
})

# Combinar los resultados en un único data frame
results_df <- do.call(rbind, results_list)


# 2. Variables cualis

library(tidyr)

# Definir las variables cualitativas a incluir en el análisis
qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", 
                                       "ESTADO_CIVIL", "AL_MENOS_UN_PAGO")

# Crear tablas de frecuencia por clúster para cada variable
frequency_tables <- lapply(qualitative_variables_of_interest, function(var) {
  table_data <- table(cartera_balanceada_original$CLUSTER, cartera_balanceada_original[[var]])
  return(as.data.frame.table(table_data))
})

# Imprimir los resultados
for (i in seq_along(qualitative_variables_of_interest)) {
  cat("Tabla de frecuencia para", qualitative_variables_of_interest[i], ":\n")
  print(frequency_tables[[i]])
  cat("\n")
}

# Combinar las tablas de frecuencia en un solo data frame
combined_frequency_table <- do.call(rbind, lapply(seq_along(qualitative_variables_of_interest), function(i) {
  data.frame(
    Variable = qualitative_variables_of_interest[i],
    frequency_tables[[i]]
  )
}))

combined_frequency_table = combined_frequency_table %>%
  rename(Cluster="Var1", Categoria="Var2")


# PASO 8: GUARDAR EL ARCHIVO FINAL

# Se guarda el archivo final
saveRDS(cartera_balanceada_original, file = "cartera_balanceada_cluster.rds")

