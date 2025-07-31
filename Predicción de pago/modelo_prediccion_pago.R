# ---
# title: "Modelo_prediccion_pago"
# author: "Georgina Ureña Ballestero"
# date: "2023-09-18"
# output: html_document
# ---
  
  
# ------------------------------ REGRESIÓN LOGÍSTICA ------------------------------ #
  
  
# --- ETAPA 1: CARGA DE LA DATA

cartera_final = readRDS(file = "cartera_final.rds")
str(cartera_final)

# Bibliotecas necesarias
library(caret)
library(dplyr)
library(glmnet)


# --- ETAPA 2: PREPROCESAMIENTO DE LA DATA

# Se estandarizan las variables numéricas utilizando el escalado estándar, también conocido como estandarización de valores. Con ello, se escalan los valores de los datos de manera que el resumen estadístico general de cada variable tenga un valor medio de cero (0) y un valor de varianza unitario (1).
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


# --- ETAPA 3: ANALISIS DE SUPUESTOS

# Prueba de independencia de todas las variables predictoras categóricas con la variable respuesta
variables_categoricas = sapply(cartera, function(x) is.factor(x))
variables_categoricas = names(variables_categoricas[variables_categoricas])

independencia_tests = lapply(variables_categoricas, function(var) {
  chisq.test(cartera_final$AL_MENOS_UN_PAGO, cartera_final[[var]])
})

# Mostrar los resultados de las pruebas de independencia
for (i in 1:length(variables_categoricas)) {
  cat("Variable:", variables_categoricas[i], "\n")
  print(independencia_tests[[i]])
}

# En todas las pruebas se obtuvo un p-value menor al nivel de significancia 0,05. Esto quiere quiere decir que existe suficiente evidencia para rechazar la hipótesis nula de independencia. En otras palabras, hay una asociación significativa entre las variables categóricas y la variable de respuesta (AL_MENOS_UN_PAGO).
# En resumen, al encontrar p-values significativamente bajos en todos los casos (menores que el nivel de significancia elegido de 0,05) en las pruebas de independencia para las variables categóricas, se puede concluir que todas las variables categóricas están relacionadas de manera significativa con la variable de respuesta AL_MENOS_UN_PAGO. Esto sugiere que las variables categóricas podrían ser predictores importantes en el modelo de regresión logística.


# Análisis de multicolinealidad (sólo para variables numéricas)
variables_numericas = sapply(cartera, is.numeric)
variables_numericas = names(variables_numericas[variables_numericas])

# Calcular la matriz de correlación entre las variables numéricas
correlation_matrix = cor(cartera[, variables_numericas])

# Crear un gráfico de matriz de correlación
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 95, tl.cex = 0.5)
# Este análisis debe hacerse sólo para las variables predictoras (x's). Aquí también aparece la variable y (AL_MENOS_UN_PAGO) porque, para poder utilizar LASSO más adelante para la regresión logística, se requiere que sea de tipo  numérico o binario (0/1) en lugar de un factor, por lo que se deja de este tipo para realizar el análisis con LASSO.  

# En la matriz de correlaciones las áreas más oscuras indican correlaciones más fuertes; esto puede ser un indicativo de multicolinealidad.
# En este caso se denotan correlaciones muy altas entre: MAO_MONTO_ORIGINAL y MAO_SALDO, MAO_MONTO_ORIGINAL y SALDO_CAPITAL, MAO_SALDO y SALDO_CAPITAL, CANTIDAD_PROMESAS_PAGO y CANTIDAD_PROMESAS_PAGO_CUMPLIDAS.
# La multicolinealidad es un problema cuando hay correlaciones fuertes entre dos o más variables independientes, ya que puede dificultar la interpretación de los coeficientes en el modelo de regresión logística. Al encontrar correlaciones fuertes, es posible que deba considerar la eliminación de una de las variables altamente correlacionadas.


# --- ETAPA 4: CODIFICACIÓN ONE-HOT PARA VARIABLES CATEGORICAS

# Para poder realizar la regresión logística se deben pasar las variables categóricas a dummies, para eso se utiliza codificación one-hot.

# Se convierten las variables categóricas en factores
cartera[variables_categoricas] <- lapply(cartera[variables_categoricas], factor)

# Se realiza la codificación one-hot para las variables categóricas
cartera_codificado = model.matrix(~ . - 1, data = cartera[, variables_categoricas])


# --- ETAPA 5: DIVISIÓN DE DATOS EN CONJUNTOS DE TRAINING Y TESTING

# Se unen las bases cartera (que tiene las x's numéricas ya escaladas) y cartera_codificado (que tiene las x's categóricas ya como dummies)
cartera = cbind((cartera %>% select(-all_of(variables_categoricas))), cartera_codificado)

set.seed(123)  # Para reproducibilidad
proporcion_entrenamiento = 0.7  # Proporción de datos de entrenamiento
indices_entrenamiento = sample(1:nrow(cartera), nrow(cartera) * proporcion_entrenamiento)
datos_entrenamiento = cartera[indices_entrenamiento, ]
datos_prueba = cartera[-indices_entrenamiento, ]

#Agrego un consecutivo en datos_prueba sólo para asegurarme más adelante de enlazar los resultados obtenidos con las variables de identificación de forma correcta
df = data.frame("INDICE" = 1:nrow(cartera)) 
df = as.data.frame(df[-indices_entrenamiento,]) 
names(df)[1]="INDICE"
datos_prueba = cbind(datos_prueba,df)


# --- ETAPA 6: CONSTRUCCIÓN DEL MODELO DE REGRESIÓN LOGÍSTICA CON LASSO

# La regresión Lasso (Least Absolute Shrinkage and Selection Operator) destaca en la prevención del sobreajuste y el manejo de datos de alta dimensionalidad. La regresión Lasso aplica un término de penalización de valor absoluto, lo que potencialmente reduce algunos coeficientes a cero, eliminando así la característica correspondiente del modelo. Este método se conoce como "regularización L1". Es una herramienta muy valiosa y útil para multicolinealidad, reducción de sobreajuste y selección de características.

# Separo las variables independientes (x's) y la variable objetivo (y)
x_entrenamiento = as.matrix(cartera[indices_entrenamiento, -(which(colnames(cartera)=="AL_MENOS_UN_PAGO"))])
y_entrenamiento = as.vector(datos_entrenamiento$AL_MENOS_UN_PAGO)

# Ajusto el modelo de regresión logística con LASSO
modelo_lasso = cv.glmnet(x_entrenamiento, y_entrenamiento, alpha = 1, nfolds = 5)

# Obtengo el valor óptimo de lambda seleccionado por validación cruzada
lambda_optimo = modelo_lasso$lambda.min

# Ajusto el modelo final con el valor óptimo de lambda
modelo_final = glmnet(x_entrenamiento, y_entrenamiento, alpha = 1, lambda = lambda_optimo, family = "binomial")

# Para saber cuáles son las variables seleccionadas como predictoras por el modelo LASSO
#coef(modelo_final)

# Guardo el modelo final para poder utilizarlo en las predicciones futuras
saveRDS(object = modelo_final, file = "modelo_final.rds")


# --- ETAPA 7: EVALUACIÓN DEL MODELO

# Utilizo el conjunto de testing para evaluar el modelo y se calculan métricas de rendimiento.
x_prueba <- as.matrix(cartera[-indices_entrenamiento, -(which(colnames(cartera)=="AL_MENOS_UN_PAGO"))])
predicciones <- predict(modelo_final, newx = x_prueba, type = "response")

# Umbral para clasificar como 1 o 0
umbral <- 0.5
predicciones_clasificadas <- ifelse(predicciones > umbral, 1, 0)

# Evalúo el modelo
matriz_confusion <- confusionMatrix(data = as.factor(predicciones_clasificadas), reference = as.factor(datos_prueba$AL_MENOS_UN_PAGO))
accuracy <- matriz_confusion$byClass["Pos Pred Value"]
recall <- matriz_confusion$byClass["Sensitivity"]
f1_score <- matriz_confusion$byClass["F1"]

# Muestro la matriz de confusión y las métricas
print(matriz_confusion)
print(accuracy)
# Accuracy (Exactitud)): Porcentaje de casos que el modelo ha acertado.
print(recall)
# Recall (Exhaustividad): Cantidad que el modelo de machine learning es capaz de identificar. 
print(f1_score)
# F1_score: Combina las medidas de precision y recall en un sólo valor. Esto es práctico porque hace más fácil el poder comparar el rendimiento combinado de la precisión y la exhaustividad entre varias soluciones.


# --- ETAPA 8: PREDICCIONES EN EL DATASET ORIGINAL

# Umbrales para clasificar probabilidades de pago 
predicciones_categorizadas = as.data.frame(predicciones)
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

datos_prueba$PROBABILIDAD = predicciones_categorizadas$PROBABILIDAD
datos_prueba$PREDICCION = predicciones_categorizadas$PREDICCION

# Uno las predicciones con las variables de identificación correspondientes
predicciones_finales =  datos_prueba %>% 
  left_join(variables_identificacion, by=c("INDICE"="INDICE")) %>%   
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, PREDICCION)


# --- ETAPA 9: EXPORTACIÓN DE RESULTADOS

# Guardar en formato RDS
saveRDS(predicciones_finales, "predicciones_finales.rds")
