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
## es muy similar a REACCION_ULTIMA_GESTION.
cartera_final = cartera_final %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION,
         CANTIDAD_HIJOS, MAO_SALDO, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO,
         CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, INTERES, MONTO_ULT_SALARIO, CANTIDAD_GESTIONES, EDAD, MONTO_ULTIMO_PAGO, EOP_DESCRIPCION,
         INS_DESCRIPCION, REACCION_ULTIMA_GESTION, TIENE_EXPEDIENTE, TIPO, SEXO, CANTIDAD_VEHICULOS, CANTIDAD_PROPIEDADES, ESTADO_CIVIL, AL_MENOS_UN_PAGO)

table(cartera_final$AL_MENOS_UN_PAGO)
cartera_final_original = as.data.frame(cartera_final)


# PASO 2: ESCALAR LAS VARIABLES CUANTITATIVAS

# Se recomienda normalizar o escalar los datos para que todas las variables tengan una influencia similar en el cálculo de la distancia. Sin embargo, en
# algunos métodos no es estrictamente necesario escalar los datos (como en el caso del clustering jerárquico), ya que el algoritmo utiliza directamente la
# distancia entre los puntos. Si las variables tienen escalas muy diferentes, puede ser útil escalarlas para mejorar la interpretación de los resultados y
# evitar que una variable domine sobre las demás en el cálculo de la distancia.

# Identificar columnas numéricas
columnas_numericas <- sapply(cartera_final, is.numeric)
cartera_final[, columnas_numericas] <- scale(cartera_final[, columnas_numericas])

# Summary de la data
skimr::skim(cartera_final) 
str(cartera_final)


# PASO 3: DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS
# Se calcularán las distancias por lotes debido a que el vector que se debe generar es muy grande para la capacidad computacional.

# Seleccionar variables que no son de tipo character
cartera_mixta <- select_if(cartera_final, function(x) !is.character(x))

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
par(mar=c(5, 4, 4, 2) + 0.1)
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

# Obtain the optimal number of clusters (assuming gap_stat is available)
optimal_clusters <- gap_stat$Tab[which.max(gap_stat$Tab[, "gap"]), "logW"]
optimal_clusters <- round(optimal_clusters, 0)
optimal_clusters

# Convert factor variables to numeric (replace 'cartera_mixta' with your actual data)
cartera_mixta_numeric <- model.matrix(~ . - 1, data = cartera_mixta)

# Function to calculate centroid distances
calculate_centroid_distances <- function(centers) {
  n <- nrow(centers)
  distances <- matrix(Inf, n, n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      distances[i, j] <- sqrt(sum((centers[i,] - centers[j,])^2))
      distances[j, i] <- distances[i, j] # Ensure symmetry
    }
  }
  return(distances)
}

# Function to merge similar clusters
merge_similar_clusters <- function(km_result, max_distance) {
  # Calculate centroid distances
  centroid_distances <- calculate_centroid_distances(km_result$centers)
  
  # Find and merge close clusters
  merged <- FALSE
  for (i in 1:(km_result$k - 1)) {
    for (j in (i + 1):km_result$k) {
      if (centroid_distances[i, j] < max_distance) {
        # Merge clusters
        km_result$centers[j, ] <- colMeans(km_result$centers[c(i, j), ])
        km_result$size[j] <- km_result$size[i] + km_result$size[j]
        km_result$cluster[km_result$cluster == i] <- j
        # Remove the merged cluster (i) from centers and size
        km_result$centers <- km_result$centers[-i, , drop = FALSE]
        km_result$size <- km_result$size[-i]
        km_result$k <- km_result$k - 1
        merged <- TRUE
        break
      }
    }
    if (merged) break
  }
  
  return(list(km_result, centroid_distances))
}

# Adjust max_distance to control cluster reduction
max_distance <- 0.01  # Experiment with different values

# Execute K-means with optimal clusters
km <- kmeans(t(dist_matrix), centers = optimal_clusters)

# Verify K-means results
if (is.null(km$centers)) {
  stop("K-means failed to converge or produce clusters.")
}

# Implement additional stopping criterion
centroid_distances <- NULL  # Initialize centroid_distances
while (nrow(km$centers) > 5 && (!is.null(centroid_distances) && mean(centroid_distances) > 0.1)) {
  result <- merge_similar_clusters(km, max_distance)
  km <- result[[1]]
  centroid_distances <- result[[2]]
  
  # Verify K-means results after merging
  if (is.null(km$centers)) {
    stop("K-means failed to converge or produce clusters after merging.")
  }
}

# Número final de clusters
final_clusters <- length(unique(km$cluster))
final_clusters


# PASO 4: APLICAR CLUSTERING JERÁRQUICO MIXTO

# Aplicar K-means con el número óptimo de clusters
set.seed(123)  # Establecer una semilla para reproducibilidad
kmeans_model <- kmeans(cartera_mixta_numeric, centers = final_clusters)

# Obtener las asignaciones de cluster para cada observación
cluster_assignments <- kmeans_model$cluster

# Agregar las asignaciones de cluster al dataframe original
cartera_mixta_con_clusters <- cbind(cartera_mixta, Cluster = cluster_assignments)


# PASO 5: AGREGAR EL CLUSTER A LA DATA ORIGINAL (cartera_final_original - sin escalar)

# Agregar la variable Cluster a cartera_final_original
cartera_final_original$CLUSTER <- cartera_mixta_con_clusters$Cluster
table(cartera_final_original$CLUSTER)


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

cartera_final_original$CLUSTER <- as.factor(cartera_final_original$CLUSTER)
str(cartera_final_original)


# 1. Variables cuantis

# Lista de variables para las que quieres calcular métricas
variables_of_interest <- c("EDAD", 
                           "CANTIDAD_HIJOS", "CANTIDAD_VEHICULOS", "CANTIDAD_PROPIEDADES", 
                           "MAO_SALDO", "MAO_MONTO_ORIGINAL", "INTERES", "MONTO_ULT_SALARIO", "MONTO_ULTIMO_PAGO", 
                           "CANTIDAD_GESTIONES", "CANTIDAD_PROMESAS_PAGO", "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS")

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
  calculate_metrics(cartera_final_original, variable)
})

# Combinar los resultados en un único data frame
results_df <- do.call(rbind, results_list)


# 2. Variables cualis

library(tidyr)

# Definir las variables cualitativas a incluir en el análisis
qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "INS_DESCRIPCION", "REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE", 
                                       "TIPO", "SEXO", "ESTADO_CIVIL", "AL_MENOS_UN_PAGO")

# Crear tablas de frecuencia por clúster para cada variable
frequency_tables <- lapply(qualitative_variables_of_interest, function(var) {
  table_data <- table(cartera_final_original$CLUSTER, cartera_final_original[[var]])
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


# PASO 8: ASIGNAR LABEL AL CLUSTER DE ACUERDO A LA DESCRIPCIÓN DE CADA UNO

# Se crean las etiquetas para cada cluster, de acuerdo a criterio propio
cartera_final_original = cartera_final_original %>%
  mutate(CLUSTER_LABEL = case_when(
    CLUSTER==1 ~ "En situación difícil",
    CLUSTER==2 ~ "Cumplidores",
    CLUSTER==3 ~ "Parece que pueden pero no quieren",
    CLUSTER==4 ~ "En proceso activo",
    CLUSTER==5 ~ "Con dificultades",
    CLUSTER==6 ~ "Con compromisos moderados",
    CLUSTER==7 ~ "En espera de acción como respuesta ",
    CLUSTER==8 ~ "Desconectados",
    CLUSTER==9 ~ "Estancados",
    CLUSTER==10 ~ "Héroes",
    TRUE ~ "revisar")) 
prop.table(table(cartera_final_original$CLUSTER_LABEL))*100


# PASO 9: GUARDAR EL ARCHIVO FINAL

# Se guarda el archivo final
saveRDS(cartera_final_original, file = "cartera_final_cluster.rds")
library(openxlsx)
write.xlsx(cartera_final_original, "cartera_final_cluster.xlsx")
