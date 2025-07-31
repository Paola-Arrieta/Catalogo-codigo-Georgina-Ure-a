# ---
# title: "prediccion_pago"
# author: "Georgina Ureña Ballestero"
# date: "2023-08-18"
# output: html_document
# editor_options: 
# chunk_output_type: inline
# ---
  
  
  # ------------------------------ REGRESIÓN LOGÍSTICA ------------------------------ #
  # --------------------------- MODELO PARA PREDICCIÓN DE PAGO --------------------------- #
  
  
# --- ETAPA 1: CARGA DE LA DATA 

cartera_final = readRDS(file = "cartera_final.rds")
# str(cartera_final)

# Bibliotecas necesarias
library(caret)
library(dplyr)
library(glmnet)


# --- ETAPA 2: PREPROCESAMIENTO DE LA DATA

# Se estandarizan las variables numéricas utilizando el escalado estándar, también conocido como estandarización de valores. Con ello, se escalan los valores de los datos de manera que el resumen estadístico general de cada variable tenga un valor medio de 0 y un valor de varianza de 1.
cartera_final$CANTIDAD_PROCESOS = scale(cartera_final$CANTIDAD_PROCESOS, center = TRUE, scale = TRUE)                   
cartera_final$CANTIDAD_HIJOS = scale(cartera_final$CANTIDAD_HIJOS, center = TRUE, scale = TRUE)                      
cartera_final$CANTIDAD_PROPIEDADES = scale(cartera_final$CANTIDAD_PROPIEDADES, center = TRUE, scale = TRUE)
cartera_final$SCORE_FINAL = scale(cartera_final$SCORE_FINAL, center = TRUE, scale = TRUE)
cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA = scale(cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA, center = TRUE, scale = TRUE) 
cartera_final$EDAD = scale(cartera_final$EDAD, center = TRUE, scale = TRUE)                             
cartera_final$MAO_MONTO_ORIGINAL = scale(cartera_final$MAO_MONTO_ORIGINAL, center = TRUE, scale = TRUE)                 
cartera_final$MAO_SALDO = scale(cartera_final$MAO_SALDO, center = TRUE, scale = TRUE)                          
cartera_final$MONTO_ULTIMO_PAGO = scale(cartera_final$MONTO_ULTIMO_PAGO, center = TRUE, scale = TRUE)                   
cartera_final$CANTIDAD_PROMESAS_PAGO = scale(cartera_final$CANTIDAD_PROMESAS_PAGO, center = TRUE, scale = TRUE)            
cartera_final$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS = scale(cartera_final$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, center = TRUE, scale = TRUE)   
cartera_final$CANTIDAD_COMUNICACIONES = scale(cartera_final$CANTIDAD_COMUNICACIONES, center = TRUE, scale = TRUE)             
cartera_final$SALDO_CAPITAL = scale(cartera_final$SALDO_CAPITAL, center = TRUE, scale = TRUE)                      
cartera_final$INTERES = scale(cartera_final$INTERES, center = TRUE, scale = TRUE)                           
cartera_final$CARGOS_PENDIENTES = scale(cartera_final$CARGOS_PENDIENTES, center = TRUE, scale = TRUE)                   
cartera_final$MONTO_ULT_SALARIO = scale(cartera_final$MONTO_ULT_SALARIO, center = TRUE, scale = TRUE)                   
cartera_final$CANTIDAD_VEHICULOS = scale(cartera_final$CANTIDAD_VEHICULOS, center = TRUE, scale = TRUE)                  
cartera_final$CANTIDAD_GESTIONES = scale(cartera_final$CANTIDAD_GESTIONES, center = TRUE, scale = TRUE)                 
cartera_final$CANTIDAD_OPERACIONES_CLIENTE = scale(cartera_final$CANTIDAD_OPERACIONES_CLIENTE, center = TRUE, scale = TRUE)        

# Se almacenan las variables de identificación por separado para eliminarlas del dataset y, posterior al análisis, ser enlazadas con los resultados que se obtengan. Además, agregro un consecutivo sólo para asegurarme de enlazar de forma correcta los resultados que se vayan a obtener con las variables de identificación.
variables_identificacion = cartera_final %>%
  select (CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION) %>%
  mutate(INDICE=1:nrow(cartera_final))

# Se eliminan las variables de identificación del conjunto de datos
cartera = cartera_final[, !(names(cartera_final) %in% c("CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO", "OPE_NUMERO_OPERACION"))]
#str(cartera)


# --- ETAPA 3: PREPARACIÓN DE LA DATA (CODIFICACIÓN ONE-HOT PARA VARIABLES CATEGORICAS)

# Para poder realizar la regresión logística se deben pasar las variables categóricas a dummies, para eso se utiliza codificación one-hot.

# Se convierten las variables categóricas en factores
variables_categoricas = sapply(cartera, function(x) is.factor(x))
variables_categoricas = names(variables_categoricas[variables_categoricas])
cartera[variables_categoricas] <- lapply(cartera[variables_categoricas], factor)

# Se realiza la codificación one-hot para las variables categóricas
cartera_codificado = model.matrix(~ . - 1, data = cartera[, variables_categoricas])

# Se unen las bases variables_numericas (que tiene las x's numéricas ya escaladas) y cartera_codificado (que tiene las x's categóricas ya como dummies)
cartera = cbind((cartera %>% select(-all_of(variables_categoricas))), cartera_codificado)

#Agrego un consecutivo en la data sólo para asegurarme más adelante de enlazar los resultados obtenidos con las variables de identificación de forma correcta
cartera = cartera %>%
  mutate("INDICE" = 1:nrow(cartera))


# --- ETAPA 4: PREDICCIONES

# Generación de predicciones en el nuevo dataset
x_nuevo <- as.matrix(cartera[, -c((which(colnames(cartera) == ("AL_MENOS_UN_PAGO"))),(which(colnames(cartera) == ("INDICE"))))])

# Se carga el modelo que se había obtenido anteriormente como el mejor
modelo_final <- readRDS("modelo_final.rds")

# Se obtienen las predicciones
predicciones_nuevo <- predict(modelo_final, newx = x_nuevo, type = "response")

# Umbrales para clasificar probabilidades de pago 
predicciones_categorizadas = as.data.frame(predicciones_nuevo)
predicciones_categorizadas = predicciones_categorizadas %>%
  rename_with(~ "PROBABILIDAD", s0) %>%
  mutate(PREDICCION = case_when(
    0 <= PROBABILIDAD & PROBABILIDAD < 0.2 ~ "Improbable",
    0.2 <= PROBABILIDAD & PROBABILIDAD < 0.4 ~ "Poco probable",
    0.4 <= PROBABILIDAD & PROBABILIDAD < 0.6 ~ "Medianamente probable",
    0.6 <= PROBABILIDAD & PROBABILIDAD < 0.8 ~ "Probable",
    0.8 <= PROBABILIDAD & PROBABILIDAD <= 1 ~ "Muy probable"))

table(predicciones_categorizadas$PREDICCION)
prop.table(table(predicciones_categorizadas$PREDICCION))*100

cartera$PROBABILIDAD = predicciones_categorizadas$PROBABILIDAD
cartera$PREDICCION = predicciones_categorizadas$PREDICCION

# Uno las predicciones con las variables de identificación correspondientes
predicciones_finales =  cartera %>% 
  left_join(variables_identificacion, by=c("INDICE"="INDICE")) %>%   
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, PREDICCION)


# --- ETAPA 5: EXPORTACIÓN DE RESULTADOS

# Guardar en formato RDS
saveRDS(predicciones_finales, "predicciones_finales.rds")
