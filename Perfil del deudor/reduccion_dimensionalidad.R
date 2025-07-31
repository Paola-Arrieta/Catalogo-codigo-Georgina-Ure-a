### TITLE: PERFIL DEL DEUDOR ###
### AUTHOR: GEORGINA UREÑA BALLESTERO ###


# Libraries
library(dplyr)
library(factoextra)
library(FactoMineR)


# --- Carga del dataset

# Carga de la data
## Dataset depurado en el análisis de predicción de pagos.
cartera_final = readRDS(file = "cartera_final.rds")
str(cartera_final)

# Formato correcto de las variables
cartera_final$AL_MENOS_UN_PAGO = as.factor(cartera_final$AL_MENOS_UN_PAGO)
cartera_final = cartera_final %>%
  mutate(AL_MENOS_UN_PAGO = dplyr::recode(AL_MENOS_UN_PAGO,
                                          "0" = "no_pago",
                                          "1" = "si_pago"))

# Summary de la data
## skim() is an alternative to summary(), quickly providing a broad overview of a data frame. It handles data of all types, dispatching a different set 
## of summary functions based on the types of columns in the data frame.
skimr::skim(cartera_final) 

# Separación de variables según su tipo
variables_id = sapply(cartera_final, function(x) is.character(x))
variables_id = names(variables_id[variables_id])

variables_cuantitativas = sapply(cartera_final, is.numeric)
variables_cuantitativas = names(variables_cuantitativas[variables_cuantitativas])

variables_cualitativas = sapply(cartera_final, function(x) is.factor(x))
variables_cualitativas = names(variables_cualitativas[variables_cualitativas])

# Data split by type
cartera_id = cartera_final[variables_id]
cartera_cuantitativa = cartera_final[variables_cuantitativas]
cartera_cualitativa = cartera_final[variables_cualitativas]

cartera_cuantitativa_id = cbind(cartera_id,cartera_cuantitativa)
cartera_cualitativa_id = cbind(cartera_id,cartera_cualitativa)




##### ----- PCA (variables cuantitativas)

# Estandarización de las variables cuantitativas (media 1 y varianza 0).
## Esto se hace porque, de no realizarse, las variables con mayor varianza dominarían al resto. 
cartera_cuantitativa = sapply(cartera_cuantitativa[,variables_cuantitativas], scale)

pca_cuantitativa = PCA(cartera_cuantitativa, graph = TRUE) 
## Con la función PCA se estandarizan automáticamente los datos, por lo que el paso anterior no era necesario. Pero se comprobó que los resultados son exactamente iguales. 
pca_cuantitativa

# Resumen de resultados
summary(pca_cuantitativa)


# Eigenvalues
pca_cuantitativa$eig
## Un eigenvalue > 1 indica que la componente principal explica más varianza de lo que lo hace una de las variables originales, estando los datos estandarizados. 
## Los componentes que cumplen con el criterio eigen > 1 son las 6 primeras. Tomando en cuenta dichas dimensiones, se acumula poco más de 60% de varianza explicada. 


# 1.Observaciones (la representación gráfica es mediante sus proyecciones):
fviz_pca_ind(pca_cuantitativa, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), #PC1 y PC2
             pointsize = 1.5)
## Dimensión 1 explica el 20.9% de la varianza, mientras que la dimensión 2 el 12.9%.


# 2. Variables (vectores - la representación gráfica es mediante sus correlaciones):

# Tabla con los resultados del análisis de componentes principales (su detalle seguidamente) 
get_pca_var(pca_cuantitativa)

# Correlación de cada variable con las dimensiones
get_pca_var(pca_cuantitativa)$cor
## Dimensión 1: MAO_MONTO_ORIGINAL, MAO_SALDO, SALDO_CAPITAL e INTERES --> Relacionado a historial crediticio del deudor.
## Dimensión 2: CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS y CANTIDAD_GESTIONES --> Relacionado al historial de pago del deudor.
## Dimensión 3: CANTIDAD_PROCESOS, SCORE_FINAL, EDAD y CANTIDAD_COMUNICACIONES --> Relacionado a la gestiones y a la edad del deudor.
## Dimensión 4: CANTIDAD_HIJOS, EDAD, CANTIDAD_VEHICULOS, CANTIDAD_PROPIEDADES y CANTIDAD_OPERACIONES_CLIENTE --> Características demográficas y adquisitivas, así como a las operaciones que tramita el deudor.
## Dimensión 5: MONTO_ULTIMO_PAGO y MONTO_ULT_SALARIO --> Relacionada a la capacidad de pago del deudor.

# Gráfico
fviz_pca_var(pca_cuantitativa, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)
## Este tipo de gráfico indica el % de varianza explicada por el primer y el segundo componente (Dim1 y Dim2, respectivamente). 
## Las variables positivamente correlacionadas se agrupan juntas o próximas, mientras que las negativamente correlacionadas se representan en lados 
## opuestos del origen o cuadrantes opuestos.
## Además, la distancia entre las variables y el origen mide la calidad de la representación de las variables (mayor cuanto más próxima a la circunferencia
## o círculo de correlación, siendo éstas las que más contribuyen en los dos primeros componentes).
## La calidad de la representación se mide por el valor al cuadrado del coseno (cos2) del ángulo del triángulo formado por el punto de origen, la observación
## y su proyección sobre el componente. Para una variable dada, la suma del cos2 sobre todos los componentes principales será igual a 1.
## Variables posicionadas cerca del origen puede ser un indicativo de que serían necesarios más de dos componentes principales para su representación. 
## Tal como se observa en el gráfico anterior, existen variables que requieren varias dimensiones o componentes para su representación, ya que son líneas
## muy cortas o  muy cercanas al origen. Pero, también, hay otras que cuyo valor del cos2 es muy alto, por lo que se encuentran muy cercanas a la
## circunferencia del dibujo.

get_pca_var(pca_cuantitativa)$contrib
## Dimensión 1: MAO_MONTO_ORIGINAL, MAO_SALDO, SALDO_CAPITAL e INTERES --> Relacionado a historial crediticio del deudor.
## Dimensión 2: CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS y CANTIDAD_GESTIONES --> Relacionado al historial de pago del deudor.
## Dimensión 3: CANTIDAD_PROCESOS, SCORE_FINAL, EDAD y CANTIDAD_COMUNICACIONES --> Relacionado a la gestiones y a la edad del deudor.
## Dimensión 4: CANTIDAD_HIJOS, EDAD, CANTIDAD_VEHICULOS, CANTIDAD_PROPIEDADES y CANTIDAD_OPERACIONES_CLIENTE --> Características demográficas y adquisitivas,
## así como a las operaciones que tramita el deudor.
## Dimensión 5: MONTO_ULTIMO_PAGO y MONTO_ULT_SALARIO --> Relacionada a la capacidad de pago del deudor. 


####### Elección del número de componentes

# Criterio de eigenvalues ordenados
fviz_screeplot(pca_cuantitativa, addlabels = TRUE, ylim = c(0,30))
## Dimensión 1: 20.9% de varianza explicada. 
## Dimensión 2: 12.9% de varianza explicada.
## Dimensión 3: 7.9% de varianza explicada.
## Dimensión 4: 7% de varianza explicada.
## Dimensión 5: 6.5% de varianza explicada.


# Contribución de variables a componentes principales 
## La línea roja discontinua indica el valor medio de la contribución. Para un determinado componente, una variable con una contribución mayor a este
## límite puede considerarse importante a la hora de aportar a esa dimensión.

# Dimensión 1: Top 10 de variables que más contribuyen a PC1
fviz_contrib(pca_cuantitativa, choice = "var", axes = 1, top = 10) #axes se refiere a la dimensión que se está evaluando 
# SALDO_CAPITAL, MAO_SALDO, MAO_MONTO_ORIGINAL e INTERES.

# Dimensión 2: Top 10 de variables que más contribuyen a PC2
fviz_contrib(pca_cuantitativa, choice = "var", axes = 2, top = 10) #axes se refiere a la dimensión que se está evaluando 
# CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS y CANTIDAD_GESTIONES.

# Dimensión 3: Top 10 de variables que más contribuyen a PC3
fviz_contrib(pca_cuantitativa, choice = "var", axes = 3, top = 10) #axes se refiere a la dimensión que se está evaluando 
# CANTIDAD_PROCESOS, EDAD, CANTIDAD_COMUNICACIONES, SCORE_FINAL, CANTIDAD_OPERACIONES_CLIENTE, CANTIDAD_HIJOS y CANTIDAD_PROPIEDADES. 

# Dimensión 4: Top 10 de variables que más contribuyen a PC4
fviz_contrib(pca_cuantitativa, choice = "var", axes = 4, top = 10) #axes se refiere a la dimensión que se está evaluando 
# CANTIDAD_HIJOS, EDAD, CANTIDAD_OPERACIONES_CLIENTE, CANTIDAD_PROPIEDADES y CANTIDAD_VEHICULOS. 

# Dimensión 5: Top 10 de variables que más contribuyen a PC5
fviz_contrib(pca_cuantitativa, choice = "var", axes = 5, top = 10) #axes se refiere a la dimensión que se está evaluando 
# MONTO_ULTIMO_PAGO, MONTO_ULT_SALARIO y CANTIDAD_HIJOS.

# Total: Top 10 de variables que más contribuyen a las primeras 5 dimensiones
fviz_contrib(pca_cuantitativa, choice = "var", axes = 1:5, top = 10) #axes se refiere a la dimensión que se está evaluando 
# MAO_SALDO, SALDO_CAPITAL, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, INTERES, CANTIDAD_GESTIONES, EDAD y MONTO_ULTIMO_PAGO.  


#### De esta forma, las variables que consideran como resultado del PCA son MAO_SALDO, SALDO_CAPITAL, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO,
## CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, INTERES, CANTIDAD_GESTIONES, EDAD y MONTO_ULTIMO_PAGO.
### De 19 variables cuantitativas que contenía el dataset  original, trabajaremos más adelante con las 9 que se obtuvieron en este análisis.


cuantitativa_final = cartera_cuantitativa_id %>%
  select(MAO_SALDO, SALDO_CAPITAL, MAO_MONTO_ORIGINAL, CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, INTERES, CANTIDAD_GESTIONES, EDAD, MONTO_ULTIMO_PAGO)




##### ----- MCA (Multiple Correspondence Analysis - MCA, para variables cualitativas)


## Se decide eliminar la variable REA_DESCRIPCION ya que es exactamente la misma que REACCION_OPERACION, así como CANTIDAD_HIJOS_CATEGORIA, ya que en el
## análisis anterior se determinó que la cantidad de hijos no es una de las variables que más contribuyen a la varianza explicada. 
## También, se quita del dataset la variable ALQ_DESCRIPCION debido a que posee 251 niveles, lo que complica y casi que convierte inmanejable la interpretación
## de los resultados que se van obteniendo).

cartera_cualitativa = cartera_cualitativa %>%
  select(-c(REA_DESCRIPCION, CANTIDAD_HIJOS_CATEGORIA, ALQ_DESCRIPCION, REACCION_ULTIMA_GESTION, REACCION_OPERACION, PRO_DESCRIPCION, INS_DESCRIPCION))


# Realizar MCA
mca_cualitativa = MCA(cartera_cualitativa, graph = TRUE)
mca_cualitativa

# Resumen de resultados
summary(mca_cualitativa)



# Eigenvalues
mca_cualitativa$eig
## No hay ningún componente que cumpla con el criterio eigen > 1. El más cercano es el de la dimensión 1 y en esta se acumula poco más de 24% de varianza explicada.  


# 1. Observaciones
fviz_mca_ind(mca_cualitativa, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), #PC1 y PC2
             pointsize = 1.5)
## Dimensión 1 explica el 24.6% de la varianza, mientras que la dimensión 2 el 8.9%.


# 2. Categorías (vectores - la representación gráfica es mediante sus correlaciones):

# Tabla con los resultados del análisis de correspondencias múltiples (su detalle seguidamente) 
get_mca_var(mca_cualitativa)

# Correlación de cada variable con las dimensiones principales 
corrplot::corrplot(get_mca_var(mca_cualitativa)$cos2, is.corr=FALSE)
## Correlaciones altas entre EOP_DESCRIPCION_DESCONOCIDO, TCT_DESCRIPCION_DESCONOCIDO, TIENE_EXPEDIENTE_Desconocido y ESTADO_CIVIL_DESCONOCIDO con la dimensión 1.
## Correlaciones media - altas entre EOP_DESCRIPCION_Inactiva, no_pago, si_pago con la dimensión 2.
## Correlaciones medias entre EOP_DESCRIPCION_Ilocalizable, FEM y MASC con la dimensión 3.
## Correlación media entre ESTADO_CIVIL_SOLTERO con la dimensión 4.
## Correlación media entre EOP_DESCRIPCION_Desconocido y TCT_DESCRIPCION_Administrada con la dimensión 5.

# Gráfico de correlación entre variables y las dimensiones principales
fviz_mca_var(mca_cualitativa, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
## Correlación alta entre SEXO, ESTADO_CIVIL, TCT_DSCRIPCION, TIENE_EXPEDIENTE Y EOP_DESCRIPCION con la dimensión 1.
## Correlación alta entre AL_MENOS_UN_PAGO Y EOP_DESCRIPCION con la dimensión 2.


# Gráfico de las coordenadas de las variables categóricas
fviz_mca_var(mca_cualitativa, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# Quality of representation of variables categories
get_mca_var(mca_cualitativa)$cos2
## Dim 1: Valores nás altos en EOP_DESCRIPCION_DESCONOCIDO, TIENE_EXPEDIENTE_Desconocido y ESTADO_CIVIL_DESCONOCIDO. 
## Dim 2: Valores más altos en EOP_DESCRIPCION_Inactiva, no_pago y si_pago.
## Dim 3: Valores más altos en EOP_DESCRIPCION_Ilocalizable, FEM y MASC.
## Dim 4: Valores más altos en TIPO_PRIVADO, TIPO_PROPIO, ESTADO_CIVIL_SOLTERO y ESTADO_CIVIL_MATRIMONIO.
## Dim 5: valores más altos en EOP_DESCRIPCION_Desconocido y TCT_DESCRIPCION_Administrada. 


# Color by cos2 values: quality on the factor map
fviz_mca_var(mca_cualitativa, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())



####### Elección del número de componentes

# Criterio de eigenvalues ordenados
fviz_screeplot(mca_cualitativa, addlabels = TRUE, ylim = c(0,30))
## Las primeras 7 dimensiones recolectan el 60.7% de la varianza explicada.


get_mca_var(mca_cualitativa)$contrib
## Dimensión 1: EOP_DESCRIPCION_DESCONOCIDO, TCT_DESCRIPCION_DESCONOCIDO, TIENE_EXPEDIENTE_Desconocido, NO INDICA (en la variable sexo) y ESTADO_CIVIL_DESCONOCIDO --> .
## Dimensión 2: si_pago, EOP_DESCRIPCION_Activa y EOP_DESCRIPCION_Cancelada --> .
## Dimensión 3: EOP_DESCRIPCION_Ilocalizable y FEM --> .
## Dimensión 4: ESTADO_CIVIL_SOLTERO, TIPO_PROPIO y ESTADO_CIVIL_MATRIMONIO --> .
## Dimensión 5: TCT_DESCRIPCION_Administrada y EOP_DESCRIPCION_Desconocido --> . 


# Contribución de variables a componentes principales 
## The red dashed line on the graph above indicates the expected average value, if the contributions were uniform. Para un determinado componente, una variable
## con una contribución mayor a este límite puede considerarse importante a la hora de aportar a esa dimensión.

# Contributions of rows to dimension 1
fviz_contrib(mca_cualitativa, choice = "var", axes = 1, top = 10) #axes=1 se refiere a la dimensión 1
## TCT_DESCRIPCION_DESCONOCIDO, EOP_DESCRIPCION_DESCONOCIDO, TIENE_EXPEDIENTE_Desconocido, ESTADO_CIVIL_DESCONOCIDO, NO INDICA, TIPO_Desconocido.

# Contributions of rows to dimension 2
fviz_contrib(mca_cualitativa, choice = "var", axes = 2, top = 10) #axes=2 se refiere a la dimensión 2
## si_pago, EOP_DESCRIPCION_Cancelada, EOP_DESCRIPCION_Activa, no_pago, TIENE_EXPEDIENTE_NO, TIENE_EXPEDIENTE_SI, EOP_DESCRIPCION_Inactiva.

# Contributions of rows to dimension 3
fviz_contrib(mca_cualitativa, choice = "var", axes = 3, top = 10) #axes=3 se refiere a la dimensión 3
## EOP_DESCRIPCION_Ilocalizable, FEM, ESTADO_CIVIL_VIUDEZ, TIPO_Desconocido, TIPO_PUBLICO, MASC, TIPO_PRIVADO.

# Contributions of rows to dimension 4
fviz_contrib(mca_cualitativa, choice = "var", axes = 4, top = 10) #axes=4 se refiere a la dimensión 4
## ESTADO_CIVIL_SOLTERO, TIPO_PROPIO, ESTADO_CIVIL_MATRIMONIO, TIPO_PRIVADO, FEM.

# Contributions of rows to dimension 5
fviz_contrib(mca_cualitativa, choice = "var", axes = 5, top = 10) #axes=5 se refiere a la dimensión 5
## EOP_DESCRIPCION_Desconocido, TCT_DESCRIPCION_Administrada.


### Descripción de cada dimensión
res.desc <- dimdesc(mca_cualitativa, axes = c(1,2))

# Description of dimension 1
res.desc[[1]]
## Variables con R2 alto (estas variables explican una mayor proporción la varianza de los datos): EOP_DESCRIPCION (0.72), TIENE_EXPEDIENTE (0.27) y 
## AL_MENOS_UN_PAGO (0.72).
## Categorías con Estimates significativos (son las que contribuyen más a la asociación entre las variables originales y la dimensión): Todas las categorías
## presentaron p-values muy bajos y, por lo tanto, son significativas.

# Description of dimension 2
res.desc[[2]]
## Variables con R2 alto (estas variables explican una mayor proporción la varianza de los datos): AL_MENOS_UN_PAGO=si_pago (0.51), 
## EOP_DESCRIPCION=EOP_DESCRIPCION_Cancelada (0.89), EOP_DESCRIPCION=EOP_DESCRIPCION_Activa (0.77), EOP_DESCRIPCION=EOP_DESCRIPCION_Desconocido (0.63).
## Categorías con Estimates significativos (son las que contribuyen más a la asociación entre las variables originales y la dimensión): Todas las categorías
## presentaron p-values muy bajos y, por lo tanto, son significativas.


cualitativa_final = cartera_cualitativa_id %>%
  select(EOP_DESCRIPCION, TCT_DESCRIPCION, TIENE_EXPEDIENTE, TIPO, SEXO, ESTADO_CIVIL, AL_MENOS_UN_PAGO)
