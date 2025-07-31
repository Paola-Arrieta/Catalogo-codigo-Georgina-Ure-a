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
sample_size <- n_after*0.1
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



# PASO 4: CREAR MATRIZ PARA VARIABLES CUANTITATVAS Y CUALITATIVAS

# Seleccionar variables cuantitativas y cualitativas
cartera_cuanti <- select_if(cartera_balanceada_sampled, is.numeric)
cartera_cuali <- select_if(cartera_balanceada_sampled, is.factor)

# Se convierten las variables cuantitativas a variables categóricas, con el fin de poder aplicar k modas con algoritmo de clusterización (se había demostrado
# anteriormente que k prototypes, aunque está diseñado para conjuntos de datos mixtos, no logró una buena distinción entre grupos; es decir, los clusters que
# generó no eran heterogéneos entre si)

# Convertir variables numéricas a rangos
cartera_cuanti_rangos <- cartera_cuanti
for (col in names(cartera_cuanti_rangos)) {
  cartera_cuanti_rangos[[col]] <- as.factor(cut(cartera_cuanti_rangos[[col]], breaks = 5))
}

# Verificar si hay valores faltantes en las variables numéricas
anyNA(cartera_cuanti_rangos)

# Combinar variables categóricas y variables numéricas imputadas
cartera_mixta <- cbind(cartera_cuali, cartera_cuanti_rangos)

# Calcular la matriz de disimilaridad con la métrica de Gower
cartera_mixta_dissimilarity <- cluster::daisy(cartera_mixta, metric = "gower")



# PASO 5: DETERMINAR EL NÚMERO ÓPTIMO DE CLÚSTERES

# Función para encontrar el punto de codo en el gráfico de la suma de cuadrados dentro de los clusters
elbow_method <- function(wss) {
  # Calcular la segunda derivada de la suma de cuadrados dentro de los clusters
  second_derivative <- diff(diff(wss))
  # Encontrar el punto de codo (mínimo de la segunda derivada)
  elbow_index <- which.min(second_derivative) + 1
  return(elbow_index)
}

# Configurar número máximo de clústeres
max_clusters <- 10

# Inicializar el vector para almacenar la suma de cuadrados dentro de los clusters
wss <- numeric(max_clusters - 1)

# Calcular la suma de cuadrados dentro de los clusters para diferentes números de clusters
for (i in 2:max_clusters) {
  kmeans_model <- kmeans(cartera_mixta_dissimilarity, centers = i)
  wss[i - 1] <- sum(kmeans_model$withinss)
}

# Encontrar el punto de codo en el gráfico de la suma de cuadrados dentro de los clusters
elbow_point <- elbow_method(wss)

# Imprimir el número óptimo de clusters
cat("Número óptimo de clusters:", elbow_point, "\n")




# PASO 7: APLICAR CLUSTERING CON EL NÚMERO ÓPTIMO DE CLÚSTERES UTILIZANDO K-MODES

# Aplicar k-modes con el número óptimo de clusters
k_modes_model <- klaR::kmodes(cartera_mixta, modes = elbow_point)

# Obtener los clusters asignados a cada observación
clusters_assignments <- k_modes_model$cluster



# PASO 8: AGREGAR EL CLUSTER A LA DATA ORIGINAL (LA MUESTRA QUE SE VIENE TRABAJANDO) 

# Agregar el cluster a los datos originales
cartera_balanceada_sampled$cluster <- clusters_assignments





# PASO 9: EVALUAR EL RENDIMIENTO DEL MODELO 

### Silhouette
## Es una métrica de evaluación de clustering que cuantifica qué tan bien están separados los clusters. 
library(cluster)

# Silhouette Score
silhouette_score <- silhouette(clusters_assignments, dist(cartera_mixta_dissimilarity))
silhouette_score_mean <- mean(silhouette_score[, 3])
silhouette_score_mean
# 0.07317054. Sugiere que hay cierta superposición o que los clusters no están bien separados. 


#### Índice de Davies-Bouldin (DBI)
## Se utiliza para evaluar la calidad de una partición en un algoritmo de clustering. Calcula la "bondad" de la partición al
## considerar tanto la dispersión dentro de cada cluster como la dispersión entre los clusters. Un valor más bajo de DBI
## indica una mejor partición, donde valores cercanos a cero son ideales.
library(clusterSim)

# Convertir variables categóricas en variables dummy
cartera_mixta_dummies <- model.matrix(~ . - 1, data = cartera_mixta)

# Calcular el Índice de Davies-Bouldin (DBI)
dbi <- index.DB(cartera_mixta_dummies, clusters_assignments)
dbi$DB
# 3.035909. Indica una partición que tiene una moderada dispersión entre los clusters y dispersión dentro de los clusters. 


### Índice Calinski-Harabasz (CH)
### Métrica interna utilizada para evaluar la calidad de una partición resultante de un algoritmo de clustering. Este índice
### mide la dispersión entre los clusters y la dispersión dentro de los clusters. Un valor CH más alto indica una mejor
## separación entre los clusters.
cluster_stats <- fpc::cluster.stats(cartera_mixta_dissimilarity, clusters_assignments)
ch <- cluster_stats$ch
ch
# 1469.808. Esto sugiere que la partición resultante tiene una buena separación entre los clusters, lo que indica que el
# algoritmo de clustering utilizado ha logrado agrupar los datos de manera efectiva en clusters distintos y bien definidos.



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
