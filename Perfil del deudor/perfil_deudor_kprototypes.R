### TITLE: PERFIL DEL DEUDOR ###
### AUTHOR: GEORGINA UREÑA BALLESTERO ###


##############
# CLUSTERING #
##############

# Es importante recordar que se había llevo a cabo un análisis previo referente a la predicción de pago de los clientes. En él se había determinado como 
# filtro que se consideraran únicamente aquellos casos en los que la variable ESTADO de la tabla "juicios" se encontraran "en trámite".
# Además, se utilizó como criterio de definición para la variable AL_MENOS_UN_PAGO (variable respuesta en el contexto de predicciones de pago) el que en la 
# variable ULTIMO_PAGO > 1980/01/01; es decir, que se hubiera realizado el último pago de esa fecha en adelante. 
# Por otro lado, la EDAD se calculó a partir de la FECHA_NACIMIENTO al día de cálculo del análisis de predicción de pago.


# PASO 1: CARGAR BIBLIOTECAS Y DATOS EN FORMATOS REQUERIDOS

# Cargar bibliotecas
library(cluster)
library(clusterCrit)
library(mclust)
library(dplyr)
library(foreach)
library(doParallel)

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

## Anteriormente se había realizado un PCA (en variables cuantitativas) para reducción de dimensiones; se utilizarán las variables seleccionadas (las que
## están comentadas son todas la variables). 
## Lo mismo con las variables cualitativas. Se había hecho un MCA para variables cualitativas, pero en este análisis se tomarán en cuenta todas excepto 
## REA_DESCRIPCION (ya que es exactamente la misma que REACCION_OPERACION), CANTIDAD_HIJOS_CATEGORIA (en el análisis anterior se determinó que la cantidad de
## hijos no es una de las variables que más contribuyen a la varianza explicada y ya se posee la cantidad de hijos numérica), ALQ_DESCRIPCION (debido a que
## tiene 251 niveles, lo que complica y casi que convierte inmanejable la interpretación de los resultados que se van obteniendo) y REACCION_OPERACION porque
## es muy similar a REACCION_ULTIMA_GESTION.
cartera_final = cartera_final %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, 
         MAO_SALDO, SALDO_CAPITAL, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO,
         CANTIDAD_PROMESAS_PAGO_CUMPLIDAS,INTERES, CANTIDAD_GESTIONES, EDAD, MONTO_ULTIMO_PAGO,
         # CANTIDAD_PROCESOS, CANTIDAD_HIJOS, CANTIDAD_SOCIEDADES_PARTICIPA, EDAD, MAO_MONTO_ORIGINAL, MAO_SALDO, MONTO_ULTIMO_PAGO, 
         # CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, CANTIDAD_COMUNICACIONES, SALDO_CAPITAL, INTERES, CARGOS_PENDIENTES, MONTO_ULT_SALARIO, 
         # CANTIDAD_VEHICULOS, CANTIDAD_PROPIEDADES, CANTIDAD_GESTIONES, CANTIDAD_OPERACIONES_CLIENTE,
         EOP_DESCRIPCION, TIENE_EXPEDIENTE, TIPO, SEXO, ESTADO_CIVIL, AL_MENOS_UN_PAGO)
# EOP_DESCRIPCION, PRO_DESCRIPCION, INS_DESCRIPCION, TCT_DESCRIPCION, REACCION_OPERACION, REACCION_ULTIMA_GESTION, TIENE_EXPEDIENTE,
# TIPO, SEXO, ESTADO_CIVIL, AL_MENOS_UN_PAGO)



# PASO 2: SUBMUESTREO DE LA CLASE MAYORITARIA. Esto implica seleccionar aleatoriamente un número igual de observaciones de la clase mayoritaria para que
# coincida con la clase minoritaria.

table(cartera_final$AL_MENOS_UN_PAGO)

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


# PASO 3: PREPROCESAR DATOS

# Reducir aún más el tamaño del conjunto de datos
sample_size <- n_after*0.01
## Ajustar este valor según capacidad de procesamiento

# Muestreo aleatorio
set.seed(123)
sample_rows <- sample(1:nrow(cartera_balanceada), size = sample_size, replace = FALSE)
cartera_balanceada_sampled <- cartera_balanceada[sample_rows, ]

table(cartera_balanceada_sampled$AL_MENOS_UN_PAGO)

# Verificar y resolver posibles duplicados en los índices de cartera_balanceada_sampled
if (any(duplicated(cartera_balanceada_sampled$OPE_NUMERO_OPERACION))) {
  warning("Se encontraron índices duplicados en cartera_balanceada_sampled. Agregando sufijo único.")
  cartera_balanceada_sampled$OPE_NUMERO_OPERACION <- make.unique(cartera_balanceada_sampled$OPE_NUMERO_OPERACION)
}

# Summary de la data
## skim() is an alternative to summary(), quickly providing a broad overview of a data frame. It handles data of all types, dispatching a different set of
## summary functions based on the types of columns in the data frame.
skimr::skim(cartera_balanceada_sampled) 

# Identificar columnas numéricas
columnas_numericas <- sapply(cartera_balanceada_sampled, is.numeric)

# Escalar sólo las variables cuantitativas
cartera_balanceada_sampled[, columnas_numericas] <- scale(cartera_balanceada_sampled[, columnas_numericas])

# Summary de la data
## skim() is an alternative to summary(), quickly providing a broad overview of a data frame. It handles data of all types, dispatching a different set of
## summary functions based on the types of columns in the data frame.
skimr::skim(cartera_balanceada_sampled) 

# Agregar una columna de identificación única a la data inicial para, más adelante, poder enlazar el cluster resultante.
cartera_balanceada_sampled$identificador <- seq_len(nrow(cartera_balanceada_sampled))
cartera_balanceada_sampled$identificador = as.character(cartera_balanceada_sampled$identificador)
## Se define como tipo character únicamente para que no afecte en la separación entre variables numéricas y tipo factor que se requiere para la creación de 
## matrices de similitud.



# PASO 4: CREAR MATRICES DE SIMILITUD/DESIGUALDAD PARA VARIABLES CUANTITATVAS Y CUALITATIVAS

# Seleccionar variables cuantitativas y cualitativas
cartera_cuanti <- select_if(cartera_balanceada_sampled, is.numeric)
cartera_cuali <- select_if(cartera_balanceada_sampled, is.factor)

# Matriz de dissimilaridad para variables cuantitativas
cuanti_dissimilarity <- daisy(cartera_cuanti, metric = "euclidean")

# Convertir variables cualitativas a factores
cartera_cuali_factor <- lapply(cartera_cuali, as.factor)
cartera_cuali_factor_df <- as.data.frame(cartera_cuali_factor, stringsAsFactors = TRUE)  # Asegurar que se traten como factores

# Matriz de dissimilaridad para variables cualitativas
cuali_dissimilarity <- cluster::daisy(cartera_cuali_factor_df, metric = "gower")



# PASO 5: INTEGRAR MATRICES DE DISSIMILARIDAD

# Combinar matrices de dissimilaridad
mixed_dissimilarity <- cbind(as.dist(cuanti_dissimilarity), cuali_dissimilarity)

# Impresiones de dimensiones clave
cat("Dimensiones de mixed_dissimilarity antes de dividir en lotes:", dim(mixed_dissimilarity), "\n")



# PASO 6: DETERMINAR EL NÚMERO ÓPTIMO DE CLÚSTERES

batch_size <- 300
num_batches <- ceiling(nrow(mixed_dissimilarity) / batch_size)

# Inicializar el vector para almacenar los resultados de cada lote
num_clusters_vector <- c()

# Inicializar registro para almacenar información de clústeres por lote
cluster_info <- character(num_batches)

# Configurar clúster paralelo
num_cores <- detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Para medir el tiempo de ejecución de todo el proceso
start_time <- Sys.time()

# Aplicar clustering por lotes
for (i in 1:num_batches) {
  start_index <- (i - 1) * batch_size + 1
  end_index <- min(i * batch_size, nrow(mixed_dissimilarity))
  
  # Verificar límites de índices
  if (start_index <= nrow(mixed_dissimilarity) && end_index <= nrow(mixed_dissimilarity)) {
    batch_data <- mixed_dissimilarity[start_index:end_index, ]
    
    # Eliminar filas con NA en el lote actual
    batch_data <- na.omit(batch_data)
    
    # Verificar si quedan suficientes datos después de eliminar NA
    if (nrow(batch_data) > 1) {
      # Aplicar clustering al lote actual
      tryCatch({
        num_clusters_batch <- NbClust::NbClust(batch_data, min.nc = 2, max.nc = 10, method = "kmeans")
        # Almacenar el número de clústeres en el vector
        num_clusters_vector[i] <- num_clusters_batch$Best.nc[1]
        # Almacenar información de clústeres en el registro
        cluster_info[i] <- paste("Lote", i, "- Número de clústeres:", num_clusters_batch$Best.nc[1])
      }, warning = function(warn) {
        # Manejar advertencias específicas si es necesario
        warning("Advertencia en lote ", i, ": ", warn)
      }, error = function(err) {
        # Manejar errores específicos si es necesario
        stop("Error en lote ", i, ": ", conditionMessage(err))
      })
      
      # Imprimir contador de lotes
      cat("Lote ", i, " de ", num_batches, " completado.\n")
    } else {
      warning("No hay suficientes datos después de eliminar los valores ausentes en el lote ", i)
    }
  }
}

# Imprimir información del número de clústeres en cada lote
cat("Información de clústeres por lote:\n")
cat(paste(cluster_info, collapse = "\n"))

# Calcular el número final de clústeres
valid_clusters <- num_clusters_vector[!is.na(num_clusters_vector)]
if (length(valid_clusters) > 0) {
  final_num_clusters <- as.numeric(names(table(valid_clusters))[which.max(table(valid_clusters))])
  # Número final de clústeres
  cat("Número final de clústeres:", final_num_clusters, "\n")
} else {
  cat("No se encontraron clústeres válidos.\n")
}

# Detener clúster paralelo
stopCluster(cl)

# Imprimir tiempo de ejecución total
end_time <- Sys.time()
cat("Tiempo total de ejecución:", end_time - start_time, "\n")



# PASO 7: APLICAR CLUSTERING CON EL NÚMERO ÓPTIMO DE CLÚSTERES UTILIZANDO K-PROTOTYPES

# Convertir la matriz de dissimilaridad en un data frame
mixed_df <- as.data.frame(mixed_dissimilarity)

# Seleccionar índices de las variables cualitativas
cualitativas_index <- which(sapply(cartera_balanceada_sampled, is.factor))

# Configurar parámetros para el algoritmo k-prototypes
num_clusters <- final_num_clusters
lambda <- NULL
iter.max <- 500
nstart <- 10

# Aplicar k-prototypes
kproto_result <- clustMixType::kproto(cartera_balanceada_sampled, num_clusters, lambda = lambda, iter.max = iter.max, nstart = nstart, categorical = cualitativas_index)

# Mostrar resultados
print(kproto_result)

# Obtener los clústeres asignados a cada observación
cluster_assignments <- kproto_result$cluster



# PASO 8: AGREGAR EL CLUSTER A LA DATA ORIGINAL (LA MUESTRA QUE SE VIENE TRABAJANDO) 

# Crear un nuevo data frame con la variable de identificación y los clusters asignados
cluster_data <- data.frame(identificador = cartera_balanceada_sampled$identificador, cluster = cluster_assignments)

# Unir los resultados al conjunto de datos original utilizando la variable de identificación
cartera_balanceada_sampled <- merge(cartera_balanceada_sampled, cluster_data, by = "identificador", all.x = TRUE)

# Verificar los primeros registros para asegurarse de que la unión fue exitosa
head(cartera_balanceada_sampled)



# PASO 9: EVALUAR EL RENDIMIENTO DEL MODELO 

### Silhouette
## Los valores de los coeficientes de Silhouette deberían estar en el rango de -1 a 1, donde los valores más cercanos a 1
## indican una buena asignación del punto a su clúster y los valores cercanos a -1 indican una mala asignación.

# Calcular el coeficiente de Silhouette para variables cuantitativas
silhouette_cuanti <- silhouette(cluster_assignments, as.dist(cuanti_dissimilarity))

# Calcular el coeficiente de Silhouette para variables cualitativas
silhouette_cuali <- silhouette(cluster_assignments, cuali_dissimilarity)

# Extraer los coeficientes de Silhouette para variables cualitativas
silhouette_cuali_values <- silhouette_cuali[, "sil_width"]

# Combinar los coeficientes de Silhouette
silhouette_combined <- cbind(silhouette_cuanti[, "sil_width"], silhouette_cuali_values)

# Calcular el coeficiente de Silhouette promedio
silhouette_avg <- mean(silhouette_combined)
silhouette_avg
# 0.085652347: Asignación no muy buena.



# PASO 10: DESCRIPCIÓN DE CADA CLUSTER

cartera_balanceada_sampled$cluster <- as.factor(cartera_balanceada_sampled$cluster)

# Data sin escalar con el cluster correspondiente
# cartera_cluster_stats <- cartera_balanceada_sampled %>%
#   left_join(cartera_balanceada,
#             by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO",
#                    "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION", "EOP_DESCRIPCION"="EOP_DESCRIPCION",
#                    "PRO_DESCRIPCION"="PRO_DESCRIPCION", "TCT_DESCRIPCION"="TCT_DESCRIPCION",
#                    "REACCION_ULTIMA_GESTION"="REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE"="TIENE_EXPEDIENTE",
#                    "TIPO"="TIPO", "SEXO"="SEXO", "ESTADO_CIVIL"="ESTADO_CIVIL", "AL_MENOS_UN_PAGO"="AL_MENOS_UN_PAGO"))
cartera_cluster_stats <- cartera_balanceada_sampled %>%
  left_join(cartera_balanceada,
            by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO",
                   "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION", "EOP_DESCRIPCION"="EOP_DESCRIPCION",
                   "TIENE_EXPEDIENTE"="TIENE_EXPEDIENTE", "TIPO"="TIPO", "SEXO"="SEXO", "ESTADO_CIVIL"="ESTADO_CIVIL",
                   "AL_MENOS_UN_PAGO"="AL_MENOS_UN_PAGO"))

# cartera_cluster_stats = cartera_cluster_stats %>%
#   select("CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO", "OPE_NUMERO_OPERACION", "EOP_DESCRIPCION", "PRO_DESCRIPCION",
#          "TCT_DESCRIPCION", "REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL", "AL_MENOS_UN_PAGO",
#          "cluster", "MAO_SALDO.y", "SALDO_CAPITAL.y",  "MAO_MONTO_ORIGINAL.y", "CANTIDAD_PROMESAS_PAGO.y",
#          "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS.y", "INTERES.y", "CANTIDAD_GESTIONES.y", "EDAD.y", "MONTO_ULTIMO_PAGO.y")
cartera_cluster_stats = cartera_cluster_stats %>%
  select("CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO", "OPE_NUMERO_OPERACION", "EOP_DESCRIPCION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL",
         "AL_MENOS_UN_PAGO", "cluster", "MAO_SALDO.y", "SALDO_CAPITAL.y",  "MAO_MONTO_ORIGINAL.y", "CANTIDAD_PROMESAS_PAGO.y",
         "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS.y", "INTERES.y", "CANTIDAD_GESTIONES.y", "EDAD.y", "MONTO_ULTIMO_PAGO.y")

cartera_cluster_stats = cartera_cluster_stats %>%
  rename(MAO_SALDO="MAO_SALDO.y", SALDO_CAPITAL="SALDO_CAPITAL.y", MAO_MONTO_ORIGINAL="MAO_MONTO_ORIGINAL.y",
         CANTIDAD_PROMESAS_PAGO="CANTIDAD_PROMESAS_PAGO.y", 
         CANTIDAD_PROMESAS_PAGO_CUMPLIDAS="CANTIDAD_PROMESAS_PAGO_CUMPLIDAS.y", INTERES="INTERES.y",
         CANTIDAD_GESTIONES="CANTIDAD_GESTIONES.y", EDAD="EDAD.y", MONTO_ULTIMO_PAGO="MONTO_ULTIMO_PAGO.y")


# Estadísticas descriptivas por cluster


# 1. Variables cuantis

# Lista de variables para las que quieres calcular métricas
variables_of_interest <- c("EDAD", "MAO_SALDO", "SALDO_CAPITAL", "MAO_MONTO_ORIGINAL", "CANTIDAD_PROMESAS_PAGO",
                           "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS", "INTERES", "CANTIDAD_GESTIONES", "MONTO_ULTIMO_PAGO")

calculate_metrics <- function(data, variable_name) {
  # Calculate range within one standard deviation of the mean
  calculate_range <- function(values) {
    mean_val <- mean(values)
    sd_val <- sd(values)
    lower_limit <- mean_val - sd_val
    upper_limit <- mean_val + sd_val
    return(paste0("(", round(lower_limit, 2), ",", round(upper_limit, 2), ")"))
  }
  
  # Calculate metrics for each cluster
  metrics <- data %>%
    group_by(cluster) %>%
    summarise(
      variable = variable_name,
      n = n(),
      total = sum(!!sym(variable_name), na.rm = TRUE),
      mean = ifelse(is.numeric(!!sym(variable_name)), mean(!!sym(variable_name), na.rm = TRUE), NA),
      median = ifelse(is.numeric(!!sym(variable_name)), median(!!sym(variable_name), na.rm = TRUE), NA),
      sd = ifelse(is.numeric(!!sym(variable_name)), sd(!!sym(variable_name), na.rm = TRUE), NA),
      min = min(!!sym(variable_name), na.rm = TRUE),
      max = max(!!sym(variable_name), na.rm = TRUE),
      range_mean_sd = calculate_range(!!rlang::eval_tidy(rlang::sym(variable_name))))
  
  return(metrics)
}

# Print the results
print(metric_results)


# 2. Variables cualis


# Definir las variables cualitativas a incluir en el análisis
# qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "PRO_DESCRIPCION", "TCT_DESCRIPCION", 
#                                        "REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL",
#                                        "AL_MENOS_UN_PAGO")
qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL", "AL_MENOS_UN_PAGO")


# Crear tablas de frecuencia por clúster para cada variable
frequency_tables <- lapply(qualitative_variables_of_interest, function(var) {
  table_data <- table(cartera_cluster_stats$cluster, cartera_cluster_stats[[var]])
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



# PASO 11: APLICAR K-PROTOTYPES A LA DATA COMPLETA (CARTERA_FINAL)

# Filtrar variables numéricas y categóricas
cartera_numerica <- select_if(cartera_final, is.numeric)
cartera_categorica <- select_if(cartera_final, is.factor)

# Configurar parámetros
num_clusters <- final_num_clusters
lambda <- NULL
iter.max <- 500
nstart <- 10
batch_size <- 1000  # Tamaño del lote

# Función para realizar el clustering en lotes
perform_clustering_batches <- function(data, num_clusters, lambda = NULL, iter.max = 100, nstart = 1, batch_size = 1000) {
  # Calcular el número total de lotes
  total_batches <- ceiling(nrow(data) / batch_size)
  
  # Inicializar lista para almacenar los resultados de clustering de cada lote
  cluster_results <- list()
  
  # Iterar sobre cada lote
  for (i in 1:total_batches) {
    # Determinar los índices de inicio y fin del lote actual
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, nrow(data))
    
    # Seleccionar el lote actual
    current_batch <- data[start_index:end_index, ]
    
    # Aplicar k-prototypes al lote actual
    kproto_result <- clustMixType::kproto(current_batch, num_clusters, lambda = lambda, iter.max = iter.max, nstart = nstart)
    
    # Almacenar los resultados de clustering del lote actual en la lista
    cluster_results[[i]] <- kproto_result$cluster
    
    # Imprimir progreso
    cat("Procesado el lote", i, "de", total_batches, "\n")
  }
  
  # Combinar los resultados de clustering de todos los lotes
  all_clusters <- unlist(cluster_results)
  
  return(all_clusters)
}

# Aplicar clustering en lotes
cluster_assignments <- perform_clustering_batches(cbind(cartera_numerica, cartera_categorica), num_clusters, lambda, iter.max, nstart, batch_size)

# Agregar el clúster a la data
cartera_final$cluster <- cluster_assignments


# PASO a: DESCRIPCIÓN DE CADA CLUSTER

cartera_final$cluster <- as.factor(cartera_final$cluster)
str(cartera_final)


# Estadísticas descriptivas por cluster

# 1. Variables cuantis

# Lista de variables para las que quieres calcular métricas
variables_of_interest <- c("EDAD", "MAO_SALDO", "SALDO_CAPITAL", "MAO_MONTO_ORIGINAL", "CANTIDAD_PROMESAS_PAGO",
                           "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS", "INTERES", "CANTIDAD_GESTIONES", "MONTO_ULTIMO_PAGO")
# variables_of_interest <- c("CANTIDAD_PROCESOS", "CANTIDAD_HIJOS", "SCORE_FINAL", "CANTIDAD_SOCIEDADES_PARTICIPA", "EDAD", "MAO_MONTO_ORIGINAL",
#                            "MAO_SALDO", "MONTO_ULTIMO_PAGO", "CANTIDAD_PROMESAS_PAGO", "CANTIDAD_COMUNICACIONES", "SALDO_CAPITAL", "INTERES", 
#                            "CARGOS_PENDIENTES", "MONTO_ULT_SALARIO", "CANTIDAD_VEHICULOS", "CANTIDAD_PROPIEDADES", "CANTIDAD_GESTIONES", 
#                            "CANTIDAD_OPERACIONES_CLIENTE")

# Function to calculate metrics, addressing grouping and ungrouping
calculate_metrics <- function(data, column, variable_name) {
  result <- data %>%
    group_by(cluster) %>%  # Explicit grouping
    summarise(
      variable = variable_name,
      n = n(),
      total = sum(!!sym(column), na.rm = TRUE),
      mean = ifelse(is.numeric(!!sym(column)),
                    mean(!!sym(column), na.rm = TRUE),
                    NA),
      median = ifelse(is.numeric(!!sym(column)),
                      median(!!sym(column), na.rm = TRUE),
                      NA),
      sd = ifelse(is.numeric(!!sym(column)),
                  sd(!!sym(column), na.rm = TRUE),
                  NA),
      min = ifelse(is.numeric(!!sym(column)),
                   min(!!sym(column), na.rm = TRUE),
                   NA),
      max = ifelse(is.numeric(!!sym(column)),
                   max(!!sym(column), na.rm = TRUE),
                   NA),
      range_for_70_percent = ifelse(is.numeric(!!sym(column)),
                                    ifelse((mean(!!sym(column), na.rm = TRUE) - sd(!!sym(column), na.rm = TRUE)) < 0,
                                           paste0("(0,", round(mean(!!sym(column), na.rm = TRUE) + sd(!!sym(column), na.rm = TRUE), 2), ")"),
                                           paste0("(", round(mean(!!sym(column), na.rm = TRUE) - sd(!!sym(column), na.rm = TRUE), 2), ",", round(mean(!!sym(column), na.rm = TRUE) + sd(!!sym(column), na.rm = TRUE), 2), ")")),
                                    NA)
    ) %>%
    ungroup() %>%     # Ungroup after calculations
    mutate(across(c("total", "mean", "median", "sd", "min", "max"), as.numeric))
  
  return(result)
}

# Calculate metrics for all variables and bind results
metric_results <- data.frame()

for (variable in variables_of_interest) {
  metrics <- calculate_metrics(cartera_cluster_stats, variable, variable)
  metric_results <- bind_rows(metric_results, metrics)
}


# 2. Variables cualis


# Definir las variables cualitativas a incluir en el análisis
# qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "PRO_DESCRIPCION", "TCT_DESCRIPCION", 
#                                        "REACCION_ULTIMA_GESTION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL",
#                                        "AL_MENOS_UN_PAGO")
qualitative_variables_of_interest <- c("EOP_DESCRIPCION", "TIENE_EXPEDIENTE", "TIPO", "SEXO", "ESTADO_CIVIL", "AL_MENOS_UN_PAGO")

# Crear tablas de frecuencia por clúster para cada variable
frequency_tables <- lapply(qualitative_variables_of_interest, function(var) {
  table_data <- table(cartera_final$cluster, cartera_final[[var]])
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


# Se guarda el archivo final
saveRDS(cartera_final, file = "cartera_final_cluster.rds")
