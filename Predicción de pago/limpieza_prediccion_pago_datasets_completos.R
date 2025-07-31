# ---
# title: "Limpieza_prediccion_pago"
# author: "Georgina Ureña Ballestero"
# date: "2023-09-18"
# output: html_document
# ---
  
  
# ------------------------------ DEFINICIÓN DEL MEJOR MODELO PARA PREDICCIÓN DE PAGO ------------------------------ #
  
  
# Paquetes

library(collapse)
library(dplyr)
library(lubridate)
library(modeest)
library(naniar)
library(readxl)
library(tidyverse)


# Carga de los datasets

# -- Cartera_estado
cartera_estado <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/cartera_estado.csv",
                           header = FALSE,
                           sep = ";",
                           dec = ".") %>% as.data.frame()
names(cartera_estado) = c("CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO", "OPE_NUMERO_OPERACION", "MAO_MONTO_ORIGINAL", "MAO_SALDO", "EOP_DESCRIPCION", "USU_NOMBRE_COMPLETO", "PRO_DESCRIPCION", "INS_DESCRIPCION", "ALQ_DESCRIPCION", "TCT_DESCRIPCION", "REACCION_OPERACION", "REA_DESCRIPCION", "REACCION_ULTIMA_GESTION", "FECHA_REACCION", "ULTIMA_GESTION", "PROXIMA_GESTION", "SUPERVISOR", "ULTIMO_PAGO", "MONTO_ULTIMO_PAGO", "CANTIDAD_PROMESAS_PAGO", "CANTIDAD_PROMESAS_PAGO_CUMPLIDAS", "CANTIDAD_COMUNICACIONES", "TIENE_EXPEDIENTE", "EXPEDIENTE", "FECHA_CARGA", "SALDO_CAPITAL", "INTERES", "CARGOS_PENDIENTES", "LUGAR_TRABAJO", "CEDULA_TRABAJO", "PRIMER_PERIODO", "PRIMER_PERIODO_SALARIO", "SEGUNDO_PERIODO", "SEGUNDO_PERIODO_SALARIO", "MONTO_ULT_SALARIO", "TIPO", "FECHA_NACIMIENTO", "UBICACIONELECTORAL", "SEXO", "CANTIDAD_VEHICULOS", "CANTIDAD_PROPIEDADES", "ESTADO_CIVIL", "CANTIDAD_GESTIONES")


# -- Hijos
hijos <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/HIJOS.csv",
                  header = TRUE,
                  sep = ";",
                  dec = ".") %>% as.data.frame()
# names(hijos) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "NOMBREHIJO") 


# -- Juicios
juicios <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/JUICIOS.csv",
                    header = TRUE,
                    sep = ";",
                    dec = ".") %>% as.data.frame()
# names(juicios) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "EXPEDIENTE", "JUZGADO", "DETALLE", "ESTADO", "FECHAINGRESO", "FECHAACTUALIZACION")


# -- Propiedades
propiedades <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/PROPIEDADES.csv",
                        header = TRUE,
                        sep = ";",
                        dec = ".") %>% as.data.frame()
# names(propiedades) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "DERECHO", "DUPLICADO", "HORIZONTAL", "NUMERO", "PLANO", "PROVINCIA", "UBICACION", "VALORFISCAL")


# -- Referencias
referencias <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/REFERENCIAS.csv",
                        header = TRUE,
                        sep = ";",
                        dec = ".") %>% as.data.frame()
# names(referencias) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "EMPRESA", "SALDO", "MONEDA", "ULTIMOPAGO", "TIPOPARTE", "FECHADESEMBOLSO", "ESTADO", "DETALLE")


# -- Sociedades
sociedades <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/SOCIEDADES.csv",
                       header = TRUE,
                       sep = ";",
                       dec = ".") %>% as.data.frame()
# names(sociedades) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "NOMBRE", "PUESTO")


# -- Vehiculos
vehiculos <- read.csv("C:/Users/gurenab/Documents/Predicción de pago/src/datasets/VEHICULOS.csv",
                      header = TRUE,
                      sep = ";",
                      dec = ".") %>% as.data.frame()
# names(vehiculos) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION", "MARCA", "MODELO", "COLOR", "ANIO", "DERECHO")
```


# Verificación de datos duplicados A NIVEL DE FILA (es decir, data completamente duplicada)
```{r}

# -- Cartera estado
nrow(cartera_estado) == nrow(distinct(cartera_estado))
# Misma cantidad (378120, no hay duplicados)


# -- Hijos
nrow(hijos) == nrow(distinct(hijos))
# Misma cantidad (592928; o sea, no existen duplicados)

head(hijos);tail(hijos)
hijos$unificado = paste0(hijos$CLI_IDENTIFICACION,hijos$OPE_NUMERO_OPERACION,"_",hijos$NOMBREHIJO)
hijos$dupli_inicial = duplicated(hijos$unificado)
hijos_duplicados = hijos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(hijos_duplicados);tail(hijos_duplicados)
hijos %>%
  filter(unificado %in% hijos_duplicados) %>%
  arrange(unificado)  # %>% View()
# Parece que, efectivamente, no hay data duplicada, por lo que no habría que recurrir a eliminarla.

hijos = hijos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(hijos$unificado), useNA = "always")
hijos = hijos %>% 
  select(-c(unificado, dupli_inicial))
head(hijos);tail(hijos)
# Quedamos en 592928 filas.


# -- Juicios
nrow(juicios) == nrow(distinct(juicios))
# Misma cantidad (797840, se supone que no hay duplicados)

head(juicios);tail(juicios)
juicios$unificado = paste0(juicios$CLI_IDENTIFICACION, "_", juicios$OPE_NUMERO_OPERACION, "_", juicios$EXPEDIENTE, "_", juicios$JUZGADO, "_",
                           juicios$DETALLE, "_", juicios$ESTADO, "_", juicios$FECHAINGRESO, "_", juicios$FECHAACTUALIZACION)
juicios$dupli_inicial = duplicated(juicios$unificado)
juicios_duplicados = juicios %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(juicios_duplicados);tail(juicios_duplicados)
juicios %>%
  filter(unificado %in% juicios_duplicados) %>%
  arrange(unificado)  # %>% View()
# Parece que, efectivamente, no hay data duplicada, por lo que no habría que recurrir a eliminarla.

juicios = juicios %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(juicios$unificado), useNA = "always")
juicios = juicios %>% 
  select(-c(unificado, dupli_inicial))
head(juicios);tail(juicios)
# Quedamos en 797840 filas.


# -- Propiedades
nrow(propiedades) == nrow(distinct(propiedades))
# Misma cantidad (99671, se supone que no hay duplicados)

head(propiedades);tail(propiedades)
propiedades$unificado = paste0(propiedades$CLI_IDENTIFICACION, "_", propiedades$OPE_NUMERO_OPERACION, "_", propiedades$DERECHO, "_",
                               propiedades$DUPLICADO, "_", propiedades$HORIZONTAL, "_", propiedades$NUMERO, "_", propiedades$PLANO, "_",
                               propiedades$PROVINCIA, "_", propiedades$UBICACION, "_", propiedades$VALORFISCAL)
propiedades$dupli_inicial = duplicated(propiedades$unificado)
propiedades_duplicados = propiedades %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(propiedades_duplicados);tail(propiedades_duplicados)
propiedades %>%
  filter(unificado %in% propiedades_duplicados) %>%
  arrange(unificado) # %>% View()
# Parece que, efectivamente, no hay data duplicada, por lo que no habría que recurrir a eliminarla.

propiedades = propiedades %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(propiedades$unificado), useNA = "always")
propiedades = propiedades %>% 
  select(-c(unificado, dupli_inicial))
head(propiedades);tail(propiedades)
# Quedamos en 99671 filas.


# -- Referencias
nrow(referencias) == nrow(distinct(referencias))
# Mismoa cantidad (498814; O sea, no existen duplicados)

head(referencias);tail(referencias)
referencias$unificado = paste0(referencias$CLI_IDENTIFICACION, "_", referencias$OPE_NUMERO_OPERACION, "_", referencias$EMPRESA, "_",
                               referencias$SALDO, "_", referencias$MONEDA, "_", referencias$ULTIMOPAGO, "_", referencias$TIPOPARTE, "_",
                               referencias$FECHADESEMBOLSO, "_",referencias$ESTADO, "_", referencias$DETALLE)
referencias$dupli_inicial = duplicated(referencias$unificado)
referencias_duplicados = referencias %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(referencias_duplicados);tail(referencias_duplicados)
referencias %>% 
  filter(unificado %in% referencias_duplicados) %>%
  arrange(unificado) # %>% View()
# Parece que, efectivamente, hay data duplicada, la cual se va a tener que eliminar.

referencias = referencias %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(referencias$unificado), useNA = "always")
referencias = referencias %>% 
  select(-c(unificado, dupli_inicial))
head(referencias);tail(referencias)
# Quedamos en 498814 filas.


# -- Sociedades
nrow(sociedades) == nrow(distinct(sociedades))
# Misma cantidad (127201; o sea, no existen duplicados)

head(sociedades);tail(sociedades)
sociedades$unificado = paste0(sociedades$CLI_IDENTIFICACION, "_", sociedades$OPE_NUMERO_OPERACION, "_", sociedades$NOMBRE, "_",
                              sociedades$PUESTO)
sociedades$dupli_inicial = duplicated(sociedades$unificado)
sociedades_duplicados = sociedades %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(sociedades_duplicados);tail(sociedades_duplicados)
sociedades %>% 
  filter(unificado %in% sociedades_duplicados) %>%
  arrange(unificado) # %>% View()
# Parece que, efectivamente, no hay data duplicada, por lo que no habría que recurrir a eliminarla.

sociedades = sociedades %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(sociedades$unificado), useNA = "always")
sociedades = sociedades %>% 
  select(-c(unificado, dupli_inicial))
head(sociedades);tail(sociedades)
# Quedamos con 127201 filas.


# -- Vehículos
nrow(vehiculos) == nrow(distinct(vehiculos))
# Misma cantidad (204476; o sea, no existen duplicados)

head(vehiculos);tail(vehiculos)
vehiculos$unificado = paste0(vehiculos$CLI_IDENTIFICACION, "_", vehiculos$MARCA, "_", vehiculos$MODELO, "_", vehiculos$COLOR, "_",
                             vehiculos$ANIO, "_", vehiculos$DERECHO)
vehiculos$dupli_inicial = duplicated(vehiculos$unificado)
vehiculos_duplicados = vehiculos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()
head(vehiculos_duplicados);tail(vehiculos_duplicados)
vehiculos %>% 
  filter(unificado %in% vehiculos_duplicados) %>%
  arrange(unificado) # %>% View()
# Parece que, efectivamente, no hay data duplicada, por lo que no habría que recurrir a eliminarla.

vehiculos = vehiculos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(vehiculos$unificado), useNA = "always")
vehiculos = vehiculos %>% 
  select(-c(unificado, dupli_inicial))
head(vehiculos);tail(vehiculos)
# Quedamos en 204476 filas.


# Verificación de nuevo en todos los datasets
nrow(cartera_estado) == nrow(distinct(cartera_estado))
nrow(hijos) == nrow(distinct(hijos))
nrow(juicios) == nrow(distinct(juicios))
nrow(propiedades) == nrow(distinct(propiedades))
nrow(referencias) == nrow(distinct(referencias))
nrow(sociedades) == nrow(distinct(sociedades))
nrow(vehiculos) == nrow(distinct(vehiculos))
# Se supone que ya no hay duplicados por fila (filas enteras duplicadas) en ninguno de los archivos


# Verificación de datos duplicados A NIVEL DE COLUMNA (es decir, data repetida en columnas importantes como número de operación)

# -- Cartera estado
head(cartera_estado)

length(cartera_estado$CLI_IDENTIFICACION) # 378120               
length(unique(cartera_estado$CLI_IDENTIFICACION)) # 353879 
# En esta es posible que hayan duplicados, ya que una misma persona puede tener varias deudas.

length(cartera_estado$CLI_NOMBRE_COMPLETO) # 378120             
length(unique(cartera_estado$CLI_NOMBRE_COMPLETO)) # 319914     
# En esta es posible que hayan duplicados, ya que una misma persona puede tener varias deudas o varios deudores tener el mismo nombre.

length(cartera_estado$OPE_NUMERO_OPERACION) # 378120             
length(unique(cartera_estado$OPE_NUMERO_OPERACION)) # 378120   
# Esta sí debe ser única porque el número sí debe estár asociado a una deuda en particular (solamente a una).


# -- Hijos
head(hijos)

length(hijos$CLI_IDENTIFICACION) # 592928                    
length(unique(hijos$CLI_IDENTIFICACION)) # 247549            
# Bajo este contexto sí tiene sentido que el número de operación se repita porque son los hijos por deudor y un deudor puede tener varios hijos.


# -- Juicios
head(juicios)    

length(juicios$CLI_IDENTIFICACION) # 797840
length(unique(juicios$CLI_IDENTIFICACION)) # 229341
# Sobre esta data no se tiene conocimiento. Sin embargo, un mismo cliente puede tener varios procesos (independientemente del estado). 


# -- Propiedades
head(propiedades)

length(propiedades$CLI_IDENTIFICACION) # 99671              
length(unique(propiedades$CLI_IDENTIFICACION)) # 74234
# Un mismo deudor puede tener varias propiedades.


# -- Referencias
head(referencias)
# Esta data se refiere a deudas en general de la persona.

length(referencias$CLI_IDENTIFICACION) # 498814                    
length(unique(referencias$CLI_IDENTIFICACION)) # 177706
# Un mismo deudor puede tener varias deudas. 


# --- Sociedades
head(sociedades)

length(sociedades$CLI_IDENTIFICACION) # 127201                   
length(unique(sociedades$CLI_IDENTIFICACION)) # 53531
# Un mismo deudor puede pertenecer a varias sociedades. 


# --- Vehículos
head(vehiculos)

length(vehiculos$CLI_IDENTIFICACION) # 204476                     
length(unique(vehiculos$CLI_IDENTIFICACION)) # 131384
# Un mismo deudor puede tener varios vehículos


# Creación de un archivo base 

# La intención acá es crear un archivo base, el cual solo contenga las variables de identificación (CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION) e ir agregando las variables que realmente pueden ser útiles en el análisis
cartera_base = cartera_estado %>% 
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION)
dim(cartera_base)  # 378120 x 3
str(cartera_base)

# Me aseguro de que el formato del nombre sea estándar
cartera_base$CLI_NOMBRE_COMPLETO = gsub("\\s+", " ", cartera_base$CLI_NOMBRE_COMPLETO)


# Trabajo sobre la data del archivo hijos

# -- Hijos
t(t(names(hijos)))
head(hijos); tail(hijos)

# CLI_IDENTIFICACION
# Cantidad de hijos por deudor
q_hijos = hijos %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_HIJOS=n()) %>% 
  #arrange(desc(CANTIDAD_HIJOS)) %>%
  as.data.frame()
head(q_hijos); tail(q_hijos)
prop.table(table(q_hijos$CANTIDAD_HIJOS))*100

mas_15_hijos = q_hijos %>%
  filter(CANTIDAD_HIJOS>=15) %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>%
  unlist() %>% 
  as.character()
mas_15_hijos[1:20]

cartera_estado %>%
  filter(CLI_IDENTIFICACION %in% mas_15_hijos)  # %>% View()
# Hay 131 casos que tienen más de 15 hijos

# Se van a plantear 2 variables para la cantidad de hijos: Una que consiste en el número de hijos tal como lo indica el dataset de hijos (66 hijos para el CLI_IDENTIFICACION CR-P-8-0063-0496-PR, por ejemplo) y otra que indica rangos.

nrow(cartera_estado) # 378120 


# Revisión de la cartera base

cartera_base = cartera_base %>% 
  left_join(q_hijos, by=c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
nrow(cartera_base) # 378120

# Los NA's se van a reemplazar con 0's
table(cartera_base$CANTIDAD_HIJOS)
cartera_base$CANTIDAD_HIJOS = replace_na(cartera_base$CANTIDAD_HIJOS, 0)
table(cartera_base$CANTIDAD_HIJOS)

# Se crean las categorías para la cantidad de hijos, de acuerdo a criterio propio
cartera_base = cartera_base %>%
  mutate(CANTIDAD_HIJOS_CATEGORIA = case_when(
    CANTIDAD_HIJOS==0 ~ "Sin hijos",
    CANTIDAD_HIJOS==1 ~ "1 hijo",
    CANTIDAD_HIJOS==2 ~ "2 hijos",
    CANTIDAD_HIJOS==3 ~ "3 hijos",
    CANTIDAD_HIJOS>=4 ~ "4 hijos o más",
    TRUE ~ "revisar")) 
prop.table(table(cartera_base$CANTIDAD_HIJOS_CATEGORIA))*100
# Hay 9.38% con 4 hijos o más.

cartera_base$CANTIDAD_HIJOS_CATEGORIA = factor(cartera_base$CANTIDAD_HIJOS_CATEGORIA,
                                               order = TRUE,
                                               levels =c("Sin hijos", "1 hijo", "2 hijos", "3 hijos","4 hijos o más"))
str(cartera_base)
head(cartera_base)
nrow(cartera_base)  # 378120
tail(cartera_base)


# Trabajo sobre la data del archivo juicios

# Juicios

# Revisando el archivo
head(juicios); tail(juicios)
str(juicios)
t(t(names(juicios)))

# CLI_IDENTIFICACION
length(juicios$CLI_IDENTIFICACION) # 797840
length(unique(juicios$CLI_IDENTIFICACION)) # 229341

# Cantidad de veces que se repite cada operación
q_juicios = juicios %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_PROCESOS=n()) %>% 
  #arrange(desc(CANTIDAD_PROCESOS)) %>%
  as.data.frame()

head(q_juicios); tail(q_juicios)
hist(q_juicios$CANTIDAD_PROCESOS)
table(q_juicios$CANTIDAD_PROCESOS)
prop.table(table(q_juicios$CANTIDAD_PROCESOS))*100

# EXPEDIENTE
juicios$EXPEDIENTE[sample(1:nrow(juicios),25)]
# El número de expediente "18-015865-1170-CJ" se interpreta como el expediente # 15865 del Juzgado 1170 de cobro judicial y demandado en el 2018. Además, entre CI (juzgado civil) y CJ (juzgado judicial) no parece existir una diferencia relevante. Es decir, el orden es: Año, número de expediente, número de juzgado y tipo de juzgado.  
# Por lo anterior, se excluye del análisis el número de expediente

# JUZGADO
juicios$JUZGADO[sample(1:nrow(juicios),25)]
length(unique(juicios$JUZGADO)) # 438 juzgados distintos
table(juicios$JUZGADO)[order(table(juicios$JUZGADO), decreasing = TRUE)]
# Cada juzgado tiene una serie de regiones a las que brinda cobertura, dependiendo de la dirección de domicilio del demandado así es el juzgado al que se debe emitir la demanda, por lo que la gran mayoría de los procesos y, por lo tanto, de los deudores con procesos, son de San José.

# DETALLE
juicios$DETALLE[sample(1:nrow(juicios),25)]
length(unique(juicios$DETALLE)) # 150 detalles distintos
table(juicios$DETALLE, useNA = "always")

# Reemplazando los NA con "No indica (NA)"
juicios$DETALLE[is.na(juicios$DETALLE)] <- "No indica (NA)"

# Reemplazando los blancos con "No indica (NA)"
juicios$DETALLE[juicios$DETALLE == ""] <- "No indica (NA)"
table(juicios$DETALLE)[order(table(juicios$DETALLE), decreasing = T)]
# En esta ocasión se excluye del análisis, pero se puede analizar y ver si incluye algo útil

# ESTADO
juicios$ESTADO[sample(1:nrow(juicios),25)]
length(unique(juicios$ESTADO)) # 62 detalles distintos
table(juicios$ESTADO, useNA = "always")

# Reemplazando los NA con "No indica (NA)"
juicios$ESTADO[is.na(juicios$ESTADO)] <- "No indica (NA)"

# Reemplazando los blancos con "No indica (NA)"
juicios$ESTADO[juicios$ESTADO == ""] <- "No indica (NA)"
table(juicios$ESTADO)[order(table(juicios$ESTADO), decreasing = TRUE)]

en_tramite = c("01. Trmite en etapa de ejecucin", "01. Trámite en etapa de ejecución", "En trmite","TRMITE", "En trámite", "TRÁMITE", "TRAMITE", "EN TRMITE", "En trÃ¡mite", "TRMITE EN ETAPA DE EJECUCION", "en tramite")
terminado = c("TERMINADO", "TERMINADO SATISFACCION EXTRAPROCESAL", "Terminado", "TERM. ACUERDO PARTES")  
inactivo = c("02. Inactivo", "INACTIVO")  
suspension = c( "34. Susp. Ejec. anterior (Pren Hip GM)", "29. Susp. Por solicitud de partes", "32. Suspensin sucesin procesal", "30. Suspensin por prejudicialidad", "33. Susp. Opos. Proc. no contenciosos", "SUS. CAUSA PENAL" ,"SUSPENDIDO")

juicios = juicios %>%
  mutate(ESTADO_JUICIOS_CATEGORIA = case_when(
    ESTADO %in% en_tramite ~ "En tramite",
    ESTADO %in% terminado ~ "Terminado",
    ESTADO %in% inactivo ~ "Inactivo",
    ESTADO %in% suspension ~ "Suspension",
    ESTADO == "ENTRADO" ~ "Entrado",
    ESTADO == "REENTRADO" ~ "Reentrado",
    ESTADO == "No indica (NA)" ~ "No indica (NA)",
    TRUE ~ "Otro")) 

table(juicios$ESTADO_JUICIOS_CATEGORIA)[order(table(juicios$ESTADO_JUICIOS_CATEGORIA), decreasing = TRUE)]
# Esta parte es muy interesante. Conversando con Don Willie, se debe filtrar por aquellos casos que se encuentran en trámite. Por lo que se realiza esa acción para continuar con el análisis.

head(juicios)
juicios_en_tramite = juicios %>%
  filter(ESTADO_JUICIOS_CATEGORIA == "En tramite") %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, ESTADO_JUICIOS_CATEGORIA) %>%
  summarise(CANTIDAD_PROCESOS=n()) 
head(juicios_en_tramite)
# Nos quedamos, entonces, con los clientes que tienen juicios en la categoría "En trámite" y con la cantidad de procesos en ese estado.

# FECHAINGRESO
length(juicios$FECHAINGRESO) # 797840
juicios$FECHAINGRESO[sample(1:nrow(juicios),25)]
# Se ven muchos blancos, por lo que se van a reemplazar con 0's
juicios$FECHAINGRESO[juicios$FECHAINGRESO == ""] <- 0 
head(table(juicios$FECHAINGRESO)[order(table(juicios$FECHAINGRESO), decreasing = TRUE)],10) # 792709 son 0's (se acaban de cambiar los blancos por 0's) y prácticamente todas las filas son blancos. Esta variable corresponde a la fecha en que se inició el proceso judicial, de acuerdo con lo entendido los primeros 2 dígitos del expediente corresponden al año.

# EXPEDIENTE
sum(is.na(juicios$EXPEDIENTE))
# Parece que no hay NA en el expediente, por lo que por lo menos se podría obtener el año. 
EXPEDIENTE_split = str_split(juicios$EXPEDIENTE, "-", n=2) 
EXPEDIENTE_AÑO =  as.data.frame(do.call(rbind,EXPEDIENTE_split))
colnames(EXPEDIENTE_AÑO) = c("AÑO","resto")
head(EXPEDIENTE_AÑO); tail(EXPEDIENTE_AÑO)
table(EXPEDIENTE_AÑO$AÑO,useNA = "always")

intervalo_2000_mas = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")

intervalo_antes_2000 = c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99") 

EXPEDIENTE_AÑO = EXPEDIENTE_AÑO %>% 
  mutate(AÑO_COMPLETO = case_when(
    AÑO %in% intervalo_2000_mas ~ paste0("20",AÑO),
    AÑO %in% intervalo_antes_2000 ~ paste0("19",AÑO),
    TRUE ~ "REVISAR"))

juicios$AÑO_INGRESO = EXPEDIENTE_AÑO$AÑO_COMPLETO
juicios %>% 
  select(EXPEDIENTE, AÑO_INGRESO) # %>% View()

# juicios_AÑOS = juicios %>% 
#   group_by(CLI_IDENTIFICACION) %>% 
#   summarise(AÑO_PRIMER_PROCESO = as.numeric(min(AÑO_INGRESO)),
#             AÑO_ULTIMO_PROCESO = as.numeric(max(AÑO_INGRESO)),
#             DIF_AÑOS = AÑO_ULTIMO_PROCESO-AÑO_PRIMER_PROCESO)
# head(juicios_AÑOS)

nrow(juicios_en_tramite)  # 217735

# FECHAACTUALIZACION
juicios$FECHAACTUALIZACION[sample(1:nrow(juicios),25)]
# Por el momento se va a omitir del análisis.

nrow(cartera_base)  # 378120
cartera_base = juicios_en_tramite %>%
  left_join(cartera_base, by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
nrow(cartera_base)  # 217735
str(cartera_base)
cartera_base = as.data.frame(cartera_base %>%
                               select (CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, ESTADO_JUICIOS_CATEGORIA, CANTIDAD_PROCESOS, CANTIDAD_HIJOS, CANTIDAD_HIJOS_CATEGORIA))

table(cartera_base$ESTADO_JUICIOS_CATEGORIA, useNA = "always")
# Hay 217735 procesos en trámite. 
nrow(cartera_base) # 217735
head(cartera_base)


# Revisión de la cartera base

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))

# CLI_IDENTIFICACION
#table(cartera_base$CLI_IDENTIFICACION,useNA = "always")
sum(is.na(cartera_base$CLI_IDENTIFICACION))
# No tiene NA's
cartera_base$CLI_IDENTIFICACION = replace_na(as.character(cartera_base$CLI_IDENTIFICACION), "Desconocido")
# En caso de haber NA's, harían sido reemplazados con Desconocido's
cartera_base$CLI_IDENTIFICACION[cartera_base$CLI_IDENTIFICACION == ""] <- "Desconocido" 
# En caso de haber blancos, habrían sido reemplazados con Desconocida,Desconocido,Desconocido's
#table(cartera_base$CLI_IDENTIFICACION,useNA = "always")


# CLI_NOMBRE_COMPLETO 
#table(cartera_base$CLI_NOMBRE_COMPLETO,useNA = "always")
sum(is.na(cartera_base$CLI_NOMBRE_COMPLETO))
# Tiene 192 NA's
cartera_base$CLI_NOMBRE_COMPLETO = replace_na(as.character(cartera_base$CLI_NOMBRE_COMPLETO), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocido's
cartera_base$CLI_NOMBRE_COMPLETO[cartera_base$CLI_NOMBRE_COMPLETO == ""] <- "Desconocido" 
# En caso de haber blancos, habrían sido reemplazados con Desconocido's
#table(cartera_base$CLI_NOMBRE_COMPLETO,useNA = "always")


# ESTADO_JUICIOS_CATEGORIA 
table(cartera_base$ESTADO_JUICIOS_CATEGORIA,useNA = "always")
sum(is.na(cartera_base$ESTADO_JUICIOS_CATEGORIA))
# No debe de haber ningún NA's
cartera_base$ESTADO_JUICIOS_CATEGORIA = replace_na(as.character(cartera_base$ESTADO_JUICIOS_CATEGORIA), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocido's
cartera_base$ESTADO_JUICIOS_CATEGORIA[cartera_base$ESTADO_JUICIOS_CATEGORIA == ""] <- "Desconocido" 
# En caso de haber blancos, habrían sido reemplazados con Desconocido's
table(cartera_base$ESTADO_JUICIOS_CATEGORIA,useNA = "always")


# CANTIDAD_PROCESOS
table(cartera_base$CANTIDAD_PROCESOS,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_PROCESOS))
# No debe de haber ningún NA's
cartera_base$CANTIDAD_PROCESOS = replace_na(cartera_base$CANTIDAD_PROCESOS, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_base$CANTIDAD_PROCESOS[cartera_base$CANTIDAD_PROCESOS == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_base$CANTIDAD_PROCESOS,useNA = "always")


# CANTIDAD_HIJOS
table(cartera_base$CANTIDAD_HIJOS,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_HIJOS))
# Hay 192 NA's
cartera_base$CANTIDAD_HIJOS= replace_na(cartera_base$CANTIDAD_HIJOS, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_base$CANTIDAD_HIJOS[cartera_base$CANTIDAD_HIJOS== ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_base$CANTIDAD_HIJOS,useNA = "always")

# CANTIDAD_HIJOS_CATEGORIA 
table(cartera_base$CANTIDAD_HIJOS_CATEGORIA,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_HIJOS_CATEGORIA))
# Hay 192 NA's
cartera_base$CANTIDAD_HIJOS_CATEGORIA = replace_na(as.character(cartera_base$CANTIDAD_HIJOS_CATEGORIA), "Sin hijos")
# En caso de haber NA's, han sido reemplazados con Sin hijos's
cartera_base$CANTIDAD_HIJOS_CATEGORIA[cartera_base$CANTIDAD_HIJOS_CATEGORIA == ""] <- "Sin hijos" 
# En caso de haber blancos, habrían sido reemplazados con Sin hijos's
table(cartera_base$CANTIDAD_HIJOS_CATEGORIA,useNA = "always")

head(cartera_base); tail(cartera_base)
nrow(cartera_base) # 217735
nrow(na.omit(cartera_base))==nrow(cartera_base)
# Misma cantidad (217735). No hay duplicados.


# Trabajo sobre la data del archivo de propiedades

# -- Propiedades


# CLI_IDENTIFICACION
length(propiedades$CLI_IDENTIFICACION) # 99671
length(unique(propiedades$CLI_IDENTIFICACION)) # 74234
# 99671 - 74234 = 25437
# Hay personas con más de una propiedad.

head(propiedades);tail(propiedades)
str(propiedades)
# Valor fiscal está definida como character cuando, en realidad, debería ser tipo numérica. Observando la data se nota que el separador de decimales es . pero los miles están separados por coma (,), por lo cual eso se debe cambiar.


# VALORFISCAL
propiedades$VALORFISCAL <- sub(",", "", propiedades$VALORFISCAL, fixed = TRUE)
propiedades$VALORFISCAL <- sub(",", "", propiedades$VALORFISCAL, fixed = TRUE)
propiedades$VALORFISCAL = as.numeric(propiedades$VALORFISCAL)
str(propiedades)

# Las variables DERECHO,	DUPLICADO, HORIZONTAL, NUMERO Y	PLANO no se van a incluir en el análisis porque no se tiene claro qué es y si podrían ser útiles en el análisis. También se elimina PROVINCIA, ya que se puede obtener con la variable UBICACION (que contiene, además de la provincia, el cantón y el distrito).

propiedades_otra = propiedades %>% 
  select(-c(DERECHO, DUPLICADO, HORIZONTAL, NUMERO, PLANO, PROVINCIA))
head(propiedades_otra)
tail(propiedades_otra)


# CLI_IDENTIFICACION
#table(propiedades_otra$CLI_IDENTIFICACION,useNA = "always")
sum(is.na(propiedades_otra$CLI_IDENTIFICACION))
# No deberían de haber NA's
propiedades_otra$CLI_IDENTIFICACION = replace_na(as.character(propiedades_otra$CLI_IDENTIFICACION), "Desconocido")
# En caso de haber NA's, harían sido reemplazados con Desconocido's
propiedades_otra$CLI_IDENTIFICACION[propiedades_otra$CLI_IDENTIFICACION == ""] <- "Desconocido" 
# En caso de haber blancos, habrían sido reemplazados con Desconocido's
#table(propiedades_otra$CLI_IDENTIFICACION,useNA = "always")


# UBICACION
#table(propiedades_otra$UBICACION,useNA = "always")
sum(is.na(propiedades_otra$UBICACION))
# Podrían haber NA's
propiedades_otra$UBICACION = replace_na(as.character(propiedades_otra$UBICACION), "Desconocida,Desconocido,Desconocido")
# En caso de haber NA's, harían sido reemplazados con Desconocido's
propiedades_otra$UBICACION[propiedades_otra$UBICACION == ""] <- "Desconocida,Desconocido,Desconocido" 
# En caso de haber blancos, habrían sido reemplazados con Desconocida,Desconocido,Desconocido's
#table(propiedades_otra$UBICACION,useNA = "always")


# VALORFISCAL
#table(propiedades_otra$VALORFISCAL,useNA = "always")
sum(is.na(propiedades_otra$VALORFISCAL))
# Hay 109 NA's
propiedades_otra$VALORFISCAL= replace_na(propiedades_otra$VALORFISCAL, 0)
# En caso de haber NA's, han sido reemplazados con 0's
propiedades_otra$VALORFISCAL[propiedades_otra$VALORFISCAL== ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(propiedades_otra$VALORFISCAL,useNA = "always")

length(propiedades_otra$VALORFISCAL[propiedades_otra$VALORFISCAL<=100000])/nrow(propiedades_otra)*100
# Notese que hay un 10.82% de las propiedades que tienen valores iguales o inferiores a los 100.0000 colones. No tiene sentido que existan propiedades con valores tan bajos, a qué se debe eso? Será qué hacen referencia a otra cosa o algo así?.

# Cantidad de propiedades
q_propiedades = propiedades_otra %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_PROPIEDADES=n()) %>% 
  arrange(desc(CANTIDAD_PROPIEDADES)) %>%
  as.data.frame()

head(q_propiedades);tail(q_propiedades)
nrow(q_propiedades) # 78862


# UBICACION 
propiedades_pcd = cbind(propiedades_otra[1],as.data.frame(do.call(rbind,str_split(propiedades_otra$UBICACION,",", n=3, simplify = FALSE))))
names(propiedades_pcd) = c("CLI_IDENTIFICACION", "PROPIEDAD_PROVINCIA", "PROPIEDAD_CANTON", "PROPIEDAD_DISTRITO")
head(propiedades_pcd)
nrow(propiedades_pcd) # 99671

# Se unen la cantidad de propiedades con la tabla de cartera_base
head(cartera_base)

cartera_base = cartera_base %>% 
  left_join(q_propiedades, by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))


# Revisión de la cartera base

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))

# CANTIDAD_PROPIEDADES
table(cartera_base$CANTIDAD_PROPIEDADES,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_PROPIEDADES))
# Hay 174188 NA's
cartera_base$CANTIDAD_PROPIEDADES= replace_na(cartera_base$CANTIDAD_PROPIEDADES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_base$CANTIDAD_PROPIEDADES[cartera_base$CANTIDAD_PROPIEDADES== ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_base$CANTIDAD_PROPIEDADES,useNA = "always")

# # VALOR_PROMEDIO_PROPIEDADES
# #table(cartera_base$VALOR_PROMEDIO_PROPIEDADES,useNA = "always")
# sum(is.na(cartera_base$VALOR_PROMEDIO_PROPIEDADES))
# # Hay 174188 NA's
# cartera_base$VALOR_PROMEDIO_PROPIEDADES= replace_na(cartera_base$VALOR_PROMEDIO_PROPIEDADES, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$VALOR_PROMEDIO_PROPIEDADES[cartera_base$VALOR_PROMEDIO_PROPIEDADES== ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# #table(cartera_base$VALOR_PROMEDIO_PROPIEDADES,useNA = "always")
# 
# # VALOR_MINIMO_PROPIEDADES
# #table(cartera_base$VALOR_MINIMO_PROPIEDADES,useNA = "always")
# sum(is.na(cartera_base$VALOR_MINIMO_PROPIEDADES))
# # Hay 174188 NA's
# cartera_base$VALOR_MINIMO_PROPIEDADES= replace_na(cartera_base$VALOR_MINIMO_PROPIEDADES, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$VALOR_MINIMO_PROPIEDADES[cartera_base$VALOR_MINIMO_PROPIEDADES== ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# #table(cartera_base$VALOR_MINIMO_PROPIEDADES,useNA = "always")
# 
# # VALOR_MAXIMO_PROPIEDADES
# #table(cartera_base$VALOR_MAXIMO_PROPIEDADES,useNA = "always")
# sum(is.na(cartera_base$VALOR_MAXIMO_PROPIEDADES))
# # Hay 174188 NA's
# cartera_base$VALOR_MAXIMO_PROPIEDADES= replace_na(cartera_base$VALOR_MAXIMO_PROPIEDADES, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$VALOR_MAXIMO_PROPIEDADES[cartera_base$VALOR_MAXIMO_PROPIEDADES== ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# #table(cartera_base$VALOR_MAXIMO_PROPIEDADES,useNA = "always")

head(cartera_base); tail(cartera_base)
nrow(cartera_base) # 217735
nrow(na.omit(cartera_base))==nrow(cartera_base)
# Misma cantidad (217735). No hay duplicados.


# Trabajo sobre la data de referencias

# -- Referencias

# Las referencias son básicamente deudas
dim(referencias) # 498814 x 10
t(t(names(referencias)))
str(referencias)
head(referencias)

# EMPRESA  
table(referencias$EMPRESA,useNA = "always")
str(referencias$EMPRESA)
sum(is.na(referencias$EMPRESA))
# No hay NA's
referencias$EMPRESA = replace_na(as.character(referencias$EMPRESA), "Desconocida")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
referencias$EMPRESA[referencias$EMPRESA== ""] <- "Desconocida" 
# En caso de haber blancos, habrían sido reemplazados con Desconocida's
table(referencias$EMPRESA,useNA = "always")

# A cuántas empresas debe
# q_empresas = referencias %>% 
#   group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
#   summarise(CANTIDAD_EMPRESAS_DEBE=n()) %>% 
#   as.data.frame()
# head(q_empresas)

# SALDO
# El saldo está definido como character cuando, en realidad, debería ser tipo numérico. Observando la data se nota que se debe al tipo de moneda en la que el cliente tiene la deuda. Ante esto, se trabaja primero convirtiendo las deudas que están en dólares a colones. 
# Además, el separador de decimales es . pero los miles están separados por coma (,), por lo cual eso se debe cambiar.
# Importante: Revisar el tipo de cambio que publica el Banco Central de Costa Rica: 540 para el 1° de setiembre del 2023.

referencias_a = referencias %>%
  filter(referencias$MONEDA=='DOLARES') 
referencias_a$SALDO_COLONES = sub(",", "", referencias_a$SALDO, fixed = TRUE)
referencias_a$SALDO_COLONES = as.numeric(referencias_a$SALDO_COLONES)  
referencias_a$SALDO_COLONES = referencias_a$SALDO_COLONES*540

referencias_b = referencias %>%
  filter(referencias$MONEDA=='COLONES')

# Debido a los problemas que está generando esta variable por los distintos formatos que posee, se decide avanzar sin ella hasta que Don Willie dé una indicación al respecto.


# MONEDA
table(referencias$MONEDA)
str(referencias$MONEDA)
# Sólo sirve para identificar y hacer la conversión de dolar a colón. Debe ser un dedazo,  hay un caso de 5000000.


# ULTIMOPAGO
#table(referencias$ULTIMOPAGO)
str(referencias$ULTIMOPAGO)
referencias$ULTIMOPAGO = as.Date(referencias$ULTIMOPAGO, format = "%Y-%m-%d")
# Se separa la fecha del último pago en día, mes y año
referencias = referencias %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  mutate(AÑO_ULTIMO_PAGO = year(ULTIMOPAGO),
         MES_ULTIMO_PAGO = month(ULTIMOPAGO),
         DIA_ULTIMO_PAGO = day(ULTIMOPAGO))
table(referencias$AÑO_ULTIMO_PAGO,useNA = "always")
table(referencias$MES_ULTIMO_PAGO,useNA = "always")
table(referencias$DIA_ULTIMO_PAGO,useNA = "always")
# Existen 132867 casos que son blancos


# TIPOPARTE
table(referencias$TIPOPARTE)
str(referencias$TIPOPARTE)
# Esta variable no me aporta en nada, se puede omitir.


# FECHADESEMBOLSO
#table(referencias$FECHADESEMBOLSO)
str(referencias$FECHADESEMBOLSO)
# Me parece que esta variable no contribuye con el análisis.


# ESTADO

table(referencias$ESTADO)
str(referencias$ESTADO)
length(unique(referencias$ESTADO)) # Hay 51 estados distintos
t(t(unique(referencias$ESTADO)))
table(referencias$ESTADO)[order(table(referencias$ESTADO),decreasing = T)]
# Se observa que existen blancos
sum(is.na(referencias$ESTADO))
# Parece que no existen NA's; pero, en caso de que los hubiera:
referencias$ESTADO = replace_na(referencias$ESTADO, "Desconocido")
# Los NA's se reemplazan con Desconocido
referencias$ESTADO[referencias$ESTADO== ""] <- "Desconocido"
# En caso de haber blancos, habrían sido reemplazados con Desconocido's
table(referencias$ESTADO)[order(table(referencias$ESTADO),decreasing = T)]

cancelado_l = c("Cancelado","CANCELADO")
administrativo_l = c("COBRO ADMINISTRATIVO","ADMINISTRATIVO")
arreglo_pago_l = c("Cuenta en Arreglo de Pago", "ARREGLO DE PAGO","ARREGLO PAGO")
legal_l = c("DEPARTAMENTO LEGAL","DEPARTAMENTO LEGAL +361", "DEPARTAMENTO LEGAL MOTOS","DEPARTAMENTO LEGAL READECUACION") 
aldia_l = c("CUENTA AL DA", "CUENTA AL DIA (02)","CUENTA AL DIA", "Cuenta al Dia")
incobrable_l = c("Cuenta InCobrable" , "INCOBRABLE", "Cuenta Incobrable" , "CUENTA INCOBRABLE", "CUENTA INCOBRABLE (07)", "Incobrable", "CONTABLEMENTE INCOBRABLE","IRRECUPERABLE")
judicial_l=c("COBRO JUDICIAL", "Cuenta en Cobro Judicial", "Cobro Judicial", "JUDICIAL" ,"CJ")                       
mora_l = c("Mora" , "MOROSIDAD","MOROSO","Cuenta Morosa", "MOROSA","CUENTA EN MORA", "Cuenta en mora","Cuenta en Mora","MORA") 
otros_l = c("1", "2", "9", "11", "403.17039999999997", "500","NO", "CUENTA CON PROBLEMAS", "PENDIENTE","NORMAL","NORMAL 0 A 30 DIAS","ESTUDIO","PERDIDAS SIGNIFICATIVAS 121 A 180 DIAS", "DE BAJA","DUDOSA RECUPERACION 180 DIAS O MAS","MANCHA EN LA PROTECTORA","Daciones en pago","ACTIVA","EN ESTUDIO", "otros")  
desconocido = c("Desconocido")

referencias = referencias %>% 
  mutate(REFERENCIA_ESTADO_AGRUPADO = case_when(
    ESTADO %in% cancelado_l ~ "Cancelado", 
    ESTADO %in% administrativo_l ~ "Administrativo", 
    ESTADO %in% arreglo_pago_l ~ "Arreglo", 
    ESTADO %in% legal_l ~ "Legal", 
    ESTADO %in% aldia_l ~ "Aldia", 
    ESTADO %in% incobrable_l ~ "Incobrable", 
    ESTADO %in% judicial_l ~ "Judicial", 
    ESTADO %in% mora_l ~ "Mora", 
    ESTADO %in% otros_l ~ "Otros", 
    ESTADO %in% desconocido ~ "Desconocido",
    TRUE ~ "Revisar"
  )
  )

table(referencias$REFERENCIA_ESTADO_AGRUPADO)[order(table(referencias$REFERENCIA_ESTADO_AGRUPADO),decreasing = T)]

# Cantidad de referencias según estado agrupado
q_referencias_estado =  referencias %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, REFERENCIA_ESTADO_AGRUPADO) %>%  
  summarise(CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO = n()) %>%
  as.data.frame()

# q_referencias_estado_traspuesta = q_referencias_estado %>%
#   pivot_wider(names_from=REFERENCIA_ESTADO_AGRUPADO, values_from = CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO, values_fill = 0)
# names(q_referencias_estado_traspuesta) = c("CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_MORA", "CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL", "CANTIDAD_REFERENCIAS_ESTADO_ARREGLO",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_OTROS", "CANTIDAD_REFERENCIAS_ESTADO_CANCELADO",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_LEGAL", "CANTIDAD_REFERENCIAS_ESTADO_ALDIA",
#                                            "CANTIDAD_REFERENCIAS_ESTADO_REVISAR", "CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO" )


## --- Creo un tipo "score" que tome como puntos positivos la cantidad de referencias que indiquen pago o señal de este y los "sume" en una puntuación final, y que tome como puntos negativos la cantidad de referencias que no indiquen pago o incumplimiento de este y los "reste" de la puntuación final. --- ##

# Puntos positivos
probando_pos = q_referencias_estado %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>%
  filter(REFERENCIA_ESTADO_AGRUPADO=='Cancelado' | REFERENCIA_ESTADO_AGRUPADO=='Arreglo' |
           REFERENCIA_ESTADO_AGRUPADO=='Aldia') %>%
  mutate(SCORE_positive = sum(CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO)) %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, SCORE_positive) 

nrow(probando_pos) == nrow(distinct(probando_pos))
# Distinta cantidad (5895 y 5787, respectivamente); o sea, existen duplicados

probando_pos$unificado = paste0(probando_pos$CLI_IDENTIFICACION, "_", probando_pos$OPE_NUMERO_OPERACION , "_",
                                probando_pos$SCORE_positive)
probando_pos$dupli_inicial = duplicated(probando_pos$unificado)
probando_pos_duplicados = probando_pos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

probando_pos %>% 
  filter(unificado %in% probando_pos_duplicados) %>%
  arrange(unificado)  # %>% View()
# Parece que, efectivamente, hay data duplicada, por lo que hay que recurrir a eliminarla.

probando_pos = probando_pos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(probando_pos$unificado), useNA = "always")
probando_pos = probando_pos %>% 
  select(-c(unificado, dupli_inicial))
# Quedamos en 5787 filas.

# Puntos negativos
probando_neg = q_referencias_estado %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>%
  filter(REFERENCIA_ESTADO_AGRUPADO=='Administrativo' | REFERENCIA_ESTADO_AGRUPADO=='Legal' |
           REFERENCIA_ESTADO_AGRUPADO=='Incobrable' | REFERENCIA_ESTADO_AGRUPADO=='Judicial' |
           REFERENCIA_ESTADO_AGRUPADO=='Mora' | REFERENCIA_ESTADO_AGRUPADO=='Otros' |
           REFERENCIA_ESTADO_AGRUPADO=='Desconocido' | REFERENCIA_ESTADO_AGRUPADO=='Revisar') %>%
  mutate(SCORE_negative = sum(CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO)) %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, SCORE_negative)

nrow(probando_neg) == nrow(distinct(probando_neg))
# Distinta cantidad (262528y 184659, respectivamente); o sea, existen duplicados

probando_neg$unificado = paste0(probando_neg$CLI_IDENTIFICACION, "_", probando_neg$OPE_NUMERO_OPERACION , "_",
                                probando_neg$SCORE_negitive)
probando_neg$dupli_inicial = duplicated(probando_neg$unificado)
probando_neg_duplicados = probando_neg %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

probando_neg %>% 
  filter(unificado %in% probando_neg_duplicados) %>%
  arrange(unificado)  # %>% View()
# Parece que, efectivamente, hay data duplicada, por lo que hay que recurrir a eliminarla.

probando_neg = probando_neg %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(probando_neg$unificado), useNA = "always")
probando_neg = probando_neg %>% 
  select(-c(unificado, dupli_inicial))
# Quedamos en 184659 filas.

# Uno ambas tablas
probando = probando_pos %>%
  full_join(probando_neg, 
            by = c('CLI_IDENTIFICACION'='CLI_IDENTIFICACION', 'OPE_NUMERO_OPERACION'='OPE_NUMERO_OPERACION'))

any_na(probando$SCORE_positive)
probando$SCORE_positive = replace_na(probando$SCORE_positive, 0)
# En caso de haber NA's en SCORE_positive, han sido reemplazados con 0's

any_na(probando$SCORE_negative)
probando$SCORE_negative = replace_na(probando$SCORE_negative, 0)
# En caso de haber NA's en SCORE_negative, han sido reemplazados con 0's  

# Se calcula la puntuación final           
otra_prueba = probando %>%
  mutate(SCORE_FINAL = SCORE_positive-SCORE_negative)

nrow(otra_prueba) == nrow(distinct(otra_prueba))
# Se comprueba nuevamente que no existen ya duplicados.      

## --- Fin del cálculo del "score" planteado.

# La tabla anterior se reduce a la puntuación final.
referencias_score = otra_prueba[,-c(3,4)]
nrow(referencias_score) # 186677
head(referencias_score)

# DETALLE
#table(referencias$DETALLE)
str(referencias$DETALLE)
# Se omite del análisis porque me parece que no aporta en nada.


# Se une lo anterior con la base para la cartera que se está formando
cartera_base = cartera_base %>%
  left_join(referencias_score, by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
t(t(names(cartera_base)))


# Revisión de la cartera base

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))


# # CANTIDAD_EMPRESAS_DEBE
# table(cartera_base$CANTIDAD_EMPRESAS_DEBE,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_EMPRESAS_DEBE))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_EMPRESAS_DEBE = replace_na(cartera_base$CANTIDAD_EMPRESAS_DEBE, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_EMPRESAS_DEBE[cartera_base$CANTIDAD_EMPRESAS_DEBE == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_EMPRESAS_DEBE,useNA = "always")


# CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ADMINISTRATIVO,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_MORA
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_MORA,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_INCOBRABLE,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_JUDICIAL,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_ARREGLO
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ARREGLO,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_OTROS
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_OTROS,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_CANCELADO
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_CANCELADO,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_LEGAL
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_LEGAL,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_ALDIA
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_ALDIA,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_REVISAR
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_REVISAR,useNA = "always")
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO == ""] <- 0
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_DESCONOCIDO,useNA = "always")


# REFERENCIA_ESTADO_AGRUPADO
# table(cartera_base$REFERENCIA_ESTADO_AGRUPADO,useNA = "always")
# sum(is.na(cartera_base$REFERENCIA_ESTADO_AGRUPADO))
# # Parece que existen 77528 NA's:
# cartera_base$REFERENCIA_ESTADO_AGRUPADO = replace_na(as.character(cartera_base$REFERENCIA_ESTADO_AGRUPADO), "Desconocido")
# # Los NA's se reemplazan con Desconocido
# cartera_base$REFERENCIA_ESTADO_AGRUPADO[cartera_base$REFERENCIA_ESTADO_AGRUPADO == ""] <- "Desconocido"
# # En caso de haber blancos, habrían sido reemplazados con Desconocido's
# table(cartera_base$REFERENCIA_ESTADO_AGRUPADO)[order(table(cartera_base$REFERENCIA_ESTADO_AGRUPADO),decreasing = T)]
# 
# 
# # CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO))
# # Hay 77528 NA's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO = replace_na(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO[cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO == ""] <- 0
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO,useNA = "always")

# SCORE_FINAL
table(cartera_base$SCORE_FINAL,useNA = "always")
sum(is.na(cartera_base$SCORE_FINAL))
# Hay 77528 NA's
cartera_base$SCORE_FINAL = replace_na(cartera_base$SCORE_FINAL, 0)
# En caso de haber NA's, han sido reemplazados con 0's
table(cartera_base$SCORE_FINAL,useNA = "always")


# Trabajo sobre la data de sociedades

# -- Sociedades

dim(sociedades) # 127201 x 4  
str(sociedades)
t(t(names(sociedades)))
head(sociedades); tail(sociedades)

# En cuántas sociedades tiene algún tipo de participación
q_sociedades = sociedades %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_SOCIEDADES_PARTICIPA = n())

# Uno la tabla anterior con la cartera_base
nrow(q_sociedades) # 58655
nrow(cartera_base) # 217735
cartera_base = cartera_base %>%
  left_join(q_sociedades, by=c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))


# Revisión de la cartera base

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))


# CANTIDAD_SOCIEDADES_PARTICIPA
table(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA))
# Hay 181017 NA's
cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA = replace_na(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA[cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA,useNA = "always")


# Trabajo sobre la data de vehículos

# -- Vehículos

# Cuántos vehículos posee 
q_vehiculos = vehiculos %>%
  group_by(CLI_IDENTIFICACION) %>% 
  summarise(CANTIDAD_VEHICULOS = n())

# Como la tabla de VEHICULOS no posee la variable OPE_NUMERO_OPERACION no se puede hacer el enlace, por lo que se decide utilizar la variable CANTIDAD_VEHICULOS de la tabla CARTERA_ESTADO, aunque no necesariamente tienen el mismo dato.
# Uno la variable anterior con la cartera_base
# cartera_base = cartera_base %>%
#   left_join(q_vehiculos, by=("CLI_IDENTIFICACION"="CLI_IDENTIFICACION"))


# Revisión del archivo cartera_base

str(cartera_base)
t(t(names(cartera_base)))


# CANTIDAD_VEHICULOS
# table(cartera_base$CANTIDAD_VEHICULOS,useNA = "always")
# sum(is.na(cartera_base$CANTIDAD_VEHICULOS))
# # Hay 128818 NA's
# cartera_base$CANTIDAD_VEHICULOS = replace_na(cartera_base$CANTIDAD_VEHICULOS, 0)
# # En caso de haber NA's, han sido reemplazados con 0's
# cartera_base$CANTIDAD_VEHICULOS[cartera_base$CANTIDAD_VEHICULOS == ""] <- 0 
# # En caso de haber blancos, habrían sido reemplazados con 0's
# table(cartera_base$CANTIDAD_VEHICULOS,useNA = "always")


# Trabajo sobre la data del archivo cartera_estado

# -- Cartera estado

# Tamaño del archivo
dim(cartera_estado) # 378120 x 44
str(cartera_estado)

# CLI_IDENTIFICACION 
str(cartera_estado$CLI_IDENTIFICACION)
# Comparando tamaño con o sin duplicados en CLI_IDENTIFICACION
length(cartera_estado$CLI_IDENTIFICACION) # 378120
length(unique(cartera_estado$CLI_IDENTIFICACION)) # 353879

# Viendo la estructura de 25 elmentos aleatorios de la variable
cartera_estado$CLI_IDENTIFICACION[sample(1:nrow(cartera_estado), 25, FALSE)]
# Se tiene que CLI_IDENTIFICACION consiste de: Sigla del país + A/P dependiendo si es administrada o propia + número de cédula + Institución.
# Nótese, sin embargo, que el número de cédula puede o no incluir guiones, v.g., "CR-P-701300370-CI" vs "CR-P-1-0656-0951-PR".

# Verificando que todas las identificaciones inicien con "CR"
CLI_IDENTIFICACION_split = str_split(cartera_estado$CLI_IDENTIFICACION, "-", n=3) # máximo 3 divisiones
ptr =  as.data.frame(do.call(rbind,CLI_IDENTIFICACION_split))
colnames(ptr) = c("pais","tipo","resto")
head(ptr); tail(ptr)

table(ptr$pais,useNA = "always")
# Casi todas las identificaciones inician con CR, sólo hay algunos casos que son NULL y ningún NA.

# Revisando cuántas de las identificaciones son Propias(P) y cuantas Administradas(A) 
table(ptr$tipo,useNA = "always")
prop.table(table(ptr$tipo,useNA = "always"))*100
# 91.09% son cuentas propias

# Revisando cuántas de las identificaciones hay por institución
ptr_institucion <- ptr %>% 
  separate(resto, sep=-2, into=c('demas', 'institucion')) %>%
  group_by(institucion) %>%
  summarise(cant=n()) %>%
  mutate(prop=(cant/sum(cant))*100)

# El 90.8% de las operaciones están asociadas a uno de los siguientes 7 de 26 bancos:
# PR 25.90%
# SB 18.34%
# CI 17.19%
# SV 13.11%
# GG 5.95%
# CX 5.79%
# DV 4.50%


# CLI_NOMBRE_COMPLETO
str(cartera_estado$CLI_NOMBRE_COMPLETO)
# Esta variable presenta desafíos especiales. Para ilustrar los problemas se ejemplifican con algunos casos:
cartera_estado_nombres = cartera_estado [c(1,2,48,52,441,129187,129223), ]
head(cartera_estado_nombres)
# En el primer caso se tiene Apellido + Apellido + Nombre. 
# En el segundo caso se tiene Nombre + Apellido + Apellido.
# En el tercer caso se tiene Apellido (con ? en lugar de ñ) + Apellido + Nombre + Nombre.
# En el cuarto caso se tiene Apellido + . + Nombre.
# En el quinto caso se tiene . + Apellido (con ? en lugar de ñ) + Nombre.
# En el sexto caso se tiene el nombre de una empresa.
# En el sétimo caso se tiene Cédula jurídica + Nombre de la sociedad.
# Por el momento esta parte no se analiza y queda pendiente de trabajar.


# OPE_NUMERO_OPERACION 
str(cartera_estado$OPE_NUMERO_OPERACION)
# Creando variable para cantidad de operaciones por identificación
# Ya se vio que la diferencia entre identificaciones e identificaciones únicas es de 378120 - 353879 = 24241. Esto se debe a que hay deudores que tienen más de una deuda.
# Se va a crear la variable Q_OPE_POR_CLIENTE para saber la cantidad de operaciones por cliente para tener en cuenta que, por ejemplo, una persona puede tener varias deudas sobre las cuales GC tiene los derechos

q_operaciones_por_cliente = cartera_estado %>%
  group_by(CLI_IDENTIFICACION) %>%
  select(OPE_NUMERO_OPERACION) %>%
  summarise(CANTIDAD_OPERACIONES_CLIENTE = n()) %>%
  as.data.frame()


# MAO_MONTO_ORIGINAL
str(cartera_estado$MAO_MONTO_ORIGINAL)
summary(cartera_estado$MAO_MONTO_ORIGINAL)
hist(cartera_estado$MAO_MONTO_ORIGINAL)
#table(cartera_estado$MAO_MONTO_ORIGINA,useNA = "always")
sum(is.na(cartera_estado$MAO_MONTO_ORIGINAL))
# No hay NA's, pero se denota que existen los valores negativos e iguales a cero
# hist(CARTERA_TOTAL$MAO_MONTO_ORIGINAL[CARTERA_TOTAL$MAO_MONTO_ORIGINAL<=0])
cartera_estado %>%
  filter(cartera_estado$MAO_MONTO_ORIGINAL<=0)  # %>% View()
# Son 401 casos.
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero. 
cartera_estado = cartera_estado %>% 
  mutate(MAO_MONTO_ORIGINAL = replace(MAO_MONTO_ORIGINAL, MAO_MONTO_ORIGINAL<0, 0))
cartera_estado$MAO_MONTO_ORIGINAL[cartera_estado$MAO_MONTO_ORIGINAL == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$MAO_MONTO_ORIGINAL,useNA = "always")


# MAO_SALDO 
str(cartera_estado$MAO_SALDO)
cor(cartera_estado$MAO_MONTO_ORIGINAL, cartera_estado$MAO_SALDO)
# Ambas variables aumentan; es decir, entre más alto el monto original, más alto el saldo.
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero.
cartera_estado = cartera_estado %>% 
  mutate(MAO_SALDO = replace(MAO_SALDO, MAO_SALDO<0, 0))
cartera_estado$MAO_SALDO[cartera_estado$MAO_SALDO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$MAO_SALDO,useNA = "always")


# EOP_DESCRIPCION
str(cartera_estado$EOP_DESCRIPCION)
#cartera_estado$EOP_DESCRIPCION
sort(table(cartera_estado$EOP_DESCRIPCION, useNA = "always"),decreasing= T)
sort(prop.table(table(cartera_estado$EOP_DESCRIPCION, useNA = "always"))*100,decreasing= T)
# El 78.82 corresponden a operaciones Inactivas, el 12.06% a Canceladas, el 6.57% a Activas y el 2.40% a Ilocalizable. Qué significa "NULL" y "excluidas".
table(cartera_estado$EOP_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$EOP_DESCRIPCION))
# No hay NA's
cartera_estado$EOP_DESCRIPCION = replace_na(as.character(cartera_estado$EOP_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$EOP_DESCRIPCION[cartera_estado$EOP_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$EOP_DESCRIPCION,useNA = "always")


# USU_NOMBRE_COMPLETO
str(cartera_estado$USU_NOMBRE_COMPLETO)
#cartera_estado$USU_NOMBRE_COMPLETO
#sort(table(cartera_estado$USU_NOMBRE_COMPLETO, useNA = "always"),decreasing= T)
#sort(prop.table(table(cartera_estado$USU_NOMBRE_COMPLETO, useNA = "always"))*100,decreasing= T)
#table(cartera_estado$USU_NOMBRE_COMPLETO,useNA = "always")
sum(is.na(cartera_estado$USU_NOMBRE_COMPLETO))
# No hay NA's
cartera_estado$USU_NOMBRE_COMPLETO = replace_na(as.character(cartera_estado$USU_NOMBRE_COMPLETO), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$USU_NOMBRE_COMPLETO[cartera_estado$USU_NOMBRE_COMPLETO == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
#table(cartera_estado$USU_NOMBRE_COMPLETO,useNA = "always")
# Esta variable no va a ser incluida en el análisis.


# PRO_DESCRIPCION
str(cartera_estado$PRO_DESCRIPCION)
#cartera_estado$PRO_DESCRIPCION
sort(table(cartera_estado$PRO_DESCRIPCION, useNA = "always"),decreasing= T)
prop.table(table(cartera_estado$PRO_DESCRIPCION, useNA = "always"))*100
table(cartera_estado$PRO_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$PRO_DESCRIPCION))
# No hay NA's
cartera_estado$PRO_DESCRIPCION = replace_na(as.character(cartera_estado$PRO_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$PRO_DESCRIPCION[cartera_estado$PRO_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$PRO_DESCRIPCION,useNA = "always")


# INS_DESCRIPCION    
str(cartera_estado$INS_DESCRIPCION)
#cartera_estado$INS_DESCRIPCION
table(cartera_estado$INS_DESCRIPCION)[order(table(cartera_estado$INS_DESCRIPCION), decreasing=TRUE)]
prop.table(table(cartera_estado$INS_DESCRIPCION)[order(table(cartera_estado$INS_DESCRIPCION), decreasing=TRUE)])*100
sort(prop.table(table(cartera_estado$INS_DESCRIPCION, useNA = "always"))*100,decreasing= T)
table(cartera_estado$INS_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$INS_DESCRIPCION))
# No hay NA's
cartera_estado$INS_DESCRIPCION = replace_na(as.character(cartera_estado$INS_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$INS_DESCRIPCION[cartera_estado$INS_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$INS_DESCRIPCION,useNA = "always")


# ALQ_DESCRIPCION  
str(cartera_estado$ALQ_DESCRIPCION)
#cartera_estado$ALQ_DESCRIPCION
#sort(table(cartera_estado$ALQ_DESCRIPCION, useNA = "always"),decreasing= T)
#sort(prop.table(table(cartera_estado$ALQ_DESCRIPCION, useNA = "always"))*100,decreasing= T)
#table(cartera_estado$ALQ_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$ALQ_DESCRIPCION))
# No hay NA's
cartera_estado$ALQ_DESCRIPCION = replace_na(as.character(cartera_estado$ALQ_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$ALQ_DESCRIPCION[cartera_estado$ALQ_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
#table(cartera_estado$ALQ_DESCRIPCION,useNA = "always")


# TCT_DESCRIPCION
str(cartera_estado$TCT_DESCRIPCION)
#cartera_estado$TCT_DESCRIPCION 
sort(table(cartera_estado$TCT_DESCRIPCION, useNA = "always"),decreasing= T)
sort(prop.table(table(cartera_estado$TCT_DESCRIPCION, useNA = "always"))*100,decreasing= T)
table(cartera_estado$TCT_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$TCT_DESCRIPCION))
# No hay NA's
cartera_estado$TCT_DESCRIPCION = replace_na(as.character(cartera_estado$TCT_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$TCT_DESCRIPCION[cartera_estado$TCT_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$TCT_DESCRIPCION,useNA = "always")


# REACCION_OPERACION
str(cartera_estado$REACCION_OPERACION)
#cartera_estado$REACCION_OPERACION
#table(cartera_estado$REACCION_OPERACION, useNA = "always")
table(cartera_estado$REACCION_OPERACION, useNA = "always")[order(table(cartera_estado$REACCION_OPERACION, useNA = "always"),decreasing=T)]
prop.table(table(cartera_estado$REACCION_OPERACION, useNA = "always")[order(table(cartera_estado$REACCION_OPERACION, useNA = "always"),decreasing=T)])*100
table(cartera_estado$REACCION_OPERACION,useNA = "always")
sum(is.na(cartera_estado$REACCION_OPERACION))
# No hay NA's
cartera_estado$REACCION_OPERACION = replace_na(as.character(cartera_estado$REACCION_OPERACION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$REACCION_OPERACION[cartera_estado$REACCION_OPERACION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$REACCION_OPERACION,useNA = "always")


# REA_DESCRIPCION 
str(cartera_estado$REA_DESCRIPCION)
#cartera_estado$REA_DESCRIPCION
#sort(table(cartera_estado$REA_DESCRIPCION, useNA = "always"),decreasing= T)
sort(prop.table(table(cartera_estado$REA_DESCRIPCION, useNA = "always"))*100,decreasing= T)
table(cartera_estado$REA_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$REA_DESCRIPCION))
# No hay NA's
cartera_estado$REA_DESCRIPCION = replace_na(as.character(cartera_estado$REA_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$REA_DESCRIPCION[cartera_estado$REA_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$REA_DESCRIPCION,useNA = "always")


# REACCION_ULTIMA_GESTION 
str(cartera_estado$REACCION_ULTIMA_GESTION)
#cartera_estado$REACCION_ULTIMA_GESTION
#table(cartera_estado$REACCION_ULTIMA_GESTION)[order(table(cartera_estado$REACCION_ULTIMA_GESTION),decreasing=TRUE)]
#prop.table(table(cartera_estado$REACCION_ULTIMA_GESTION)[order(table(cartera_estado$REACCION_ULTIMA_GESTION),decreasing=TRUE)])*100
#sort(prop.table(table(cartera_estado$REACCION_ULTIMA_GESTION, useNA = "always"))*100,decreasing= T)
#table(cartera_estado$REACCION_ULTIMA_GESTION,useNA = "always")
sum(is.na(cartera_estado$REACCION_ULTIMA_GESTION))
# No hay NA's
cartera_estado$REACCION_ULTIMA_GESTION = replace_na(as.character(cartera_estado$REACCION_ULTIMA_GESTION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$REACCION_ULTIMA_GESTION[cartera_estado$REACCION_ULTIMA_GESTION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$REACCION_ULTIMA_GESTION,useNA = "always")


# FECHA_REACCION   
str(cartera_estado$FECHA_REACCION)
#cartera_estado$FECHA_REACCION
# No se va a incluir


# ULTIMA_GESTION
str(cartera_estado$ULTIMA_GESTION)
#cartera_estado$ULTIMA_GESTION
# No se va a incluir


# PROXIMA_GESTION 
str(cartera_estado$PROXIMA_GESTION)
#cartera_estado$PROXIMA_GESTION
# No se va a incluir


# SUPERVISOR
str(cartera_estado$SUPERVISOR)
#cartera_estado$SUPERVISOR
# No se va a incluir


# ULTIMO_PAGO
str(cartera_estado$ULTIMO_PAGO)
#cartera_estado$ULTIMO_PAGO
# No se va a incluir como tal, si no que se va a trabajar para que sea la variable respuesta.

# AL_MENOS_UN_PAGO
cartera_estado$ULTIMO_PAGO = as.Date(cartera_estado$ULTIMO_PAGO,format="%Y-%m-%d")

al_menos_un_pago = cartera_estado %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, ULTIMO_PAGO) %>%
  mutate(AL_MENOS_UN_PAGO = case_when(
    ULTIMO_PAGO>as.Date("1980/1/1", format ="%Y/%m/%d") ~ "1",
    TRUE ~ "0"
  ))
table(al_menos_un_pago$AL_MENOS_UN_PAGO)
prop.table(table(al_menos_un_pago$AL_MENOS_1_PAGO))*100
al_menos_un_pago = al_menos_un_pago[,-c(4)]


# MONTO_ULTIMO_PAGO
str(cartera_estado$MONTO_ULTIMO_PAGO)
cartera_estado$MONTO_ULTIMO_PAGO[sample(1:nrow(cartera_estado), 25, FALSE)]
cartera_estado$MONTO_ULTIMO_PAGO = as.numeric(cartera_estado$MONTO_ULTIMO_PAGO)
summary(cartera_estado$MONTO_ULTIMO_PAGO)
sum(is.na(cartera_estado$MONTO_ULTIMO_PAGO))
#table(cartera_estado$MONTO_ULTIMO_PAGO,useNA = "always")
sum(is.na(cartera_estado$MONTO_ULTIMO_PAGO))
# Hay 287593 NA's
cartera_estado$MONTO_ULTIMO_PAGO = replace_na(cartera_estado$MONTO_ULTIMO_PAGO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$MONTO_ULTIMO_PAGO[cartera_estado$MONTO_ULTIMO_PAGO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$MONTO_ULTIMO_PAGO,useNA = "always")


# CANTIDAD_PROMESAS_PAGO
str(cartera_estado$CANTIDAD_PROMESAS_PAGO)
#cartera_estado$CANTIDAD_PROMESAS_PAGO
#table(cartera_estado$CANTIDAD_PROMESAS_PAGO)
sum(is.na(cartera_estado$CANTIDAD_PROMESAS_PAGO))
# No hay NA's
cartera_estado$CANTIDAD_PROMESAS_PAGO = replace_na(cartera_estado$CANTIDAD_PROMESAS_PAGO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROMESAS_PAGO[cartera_estado$CANTIDAD_PROMESAS_PAGO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$CANTIDAD_PROMESAS_PAGO,useNA = "always")


# CANTIDAD_PROMESAS_PAGO_CUMPLIDAS
str(cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS)
#cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS
table(cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS)
sum(is.na(cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS))
# No hay NA's
cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS = replace_na(cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS[cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS,useNA = "always")


# CANTIDAD_COMUNICACIONES
str(cartera_estado$CANTIDAD_COMUNICACIONES)
#cartera_estado$CANTIDAD_COMUNICACIONES
table(cartera_estado$CANTIDAD_COMUNICACIONES)
sum(is.na(cartera_estado$CANTIDAD_COMUNICACIONES))
# No hay NA's
cartera_estado$CANTIDAD_COMUNICACIONES = replace_na(cartera_estado$CANTIDAD_COMUNICACIONES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_COMUNICACIONES[cartera_estado$CANTIDAD_COMUNICACIONES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_COMUNICACIONES,useNA = "always")


# TIENE_EXPEDIENTE 
str(cartera_estado$TIENE_EXPEDIENTE)
#cartera_estado$TIENE_EXPEDIENTE 
table(cartera_estado$TIENE_EXPEDIENTE,useNA = "always")
sum(is.na(cartera_estado$TIENE_EXPEDIENTE))
# No hay NA's
cartera_estado$TIENE_EXPEDIENTE = replace_na(as.character(cartera_estado$TIENE_EXPEDIENTE), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$TIENE_EXPEDIENTE[cartera_estado$TIENE_EXPEDIENTE == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$TIENE_EXPEDIENTE,useNA = "always")


# EXPEDIENTE
str(cartera_estado$EXPEDIENTE)
#cartera_estado$EXPEDIENTE
# No se va a incluir.


# FECHA_CARGA 
str(cartera_estado$FECHA_CARGA)
#cartera_estado$FECHA_CARGA
# No se va a incluir.


# SALDO_CAPITAL 
str(cartera_estado$SALDO_CAPITAL)
#cartera_estado$SALDO_CAPITAL
cartera_estado$SALDO_CAPITAL = sub(",", ".", cartera_estado$SALDO_CAPITAL, fixed = TRUE)
cartera_estado$SALDO_CAPITAL = as.numeric(cartera_estado$SALDO_CAPITAL)
#table(cartera_estado$SALDO_CAPITAL,useNA = "always")
sum(is.na(cartera_estado$SALDO_CAPITAL))
# No hay NA's
cartera_estado$SALDO_CAPITAL = replace_na(cartera_estado$SALDO_CAPITAL, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$SALDO_CAPITAL[cartera_estado$SALDO_CAPITAL == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$SALDO_CAPITAL,useNA = "always")
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero. 
cartera_estado = cartera_estado %>% 
  mutate(SALDO_CAPITAL = replace(SALDO_CAPITAL, SALDO_CAPITAL<0, 0))
#table(cartera_estado$SALDO_CAPITAL,useNA = "always")


# INTERES 
str(cartera_estado$INTERES)
#cartera_estado$INTERES
cartera_estado$INTERES = sub(",", ".", cartera_estado$INTERES, fixed = TRUE)
cartera_estado$INTERES = as.numeric(cartera_estado$INTERES)
#table(cartera_estado$INTERES,useNA = "always")
sum(is.na(cartera_estado$INTERES))
# Hay 1 NA's, pero se denota que existen los valores negativos e iguales a cero
cartera_estado %>%
  filter(cartera_estado$INTERES<=0)  # %>% View()
# Son 46591 casos.
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero. 
cartera_estado = cartera_estado %>% 
  mutate(INTERES = replace(INTERES, INTERES<0, 0))
cartera_estado$INTERES[cartera_estado$INTERES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$INTERES,useNA = "always")


# CARGOS_PENDIENTES    
str(cartera_estado$CARGOS_PENDIENTES)
#cartera_estado$CARGOS_PENDIENTES
cartera_estado$CARGOS_PENDIENTES = sub(",", ".", cartera_estado$CARGOS_PENDIENTES, fixed = TRUE)
cartera_estado$CARGOS_PENDIENTES = as.numeric(cartera_estado$CARGOS_PENDIENTES)
#table(cartera_estado$CARGOS_PENDIENTES,useNA = "always")
sum(is.na(cartera_estado$CARGOS_PENDIENTES))
# No hay NA's
cartera_estado$CARGOS_PENDIENTES = replace_na(cartera_estado$CARGOS_PENDIENTES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CARGOS_PENDIENTES[cartera_estado$CARGOS_PENDIENTES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$CARGOS_PENDIENTES,useNA = "always")


# LUGAR_TRABAJO 
str(cartera_estado$LUGAR_TRABAJO)
#table(cartera_estado$LUGAR_TRABAJO)
# No se va a incluir pero sí parece interesante.


# CEDULA_TRABAJO
str(cartera_estado$CEDULA_TRABAJO)
#table(cartera_estado$CEDULA_TRABAJO)
# No se va a incluir.  


# PRIMER_PERIODO  
str(cartera_estado$PRIMER_PERIODO)
#table(cartera_estado$PRIMER_PERIODO)
# No se va a incluir.


# PRIMER_PERIODO_SALARIO 
str(cartera_estado$PRIMER_PERIODO_SALARIO)
#table(cartera_estado$PRIMER_PERIODO_SALARIO)
# No se va a incluir.


# SEGUNDO_PERIODO  
str(cartera_estado$SEGUNDO_PERIODO)
#table(cartera_estado$SEGUNDO_PERIODO)
# No se va a incluir.


# SEGUNDO_PERIODO_SALARIO
str(cartera_estado$SEGUNDO_PERIODO_SALARIO)
#table(cartera_estado$SEGUNDO_PERIODO_SALARIO)
# No se va a incluir.


# MONTO_ULT_SALARIO
str(cartera_estado$MONTO_ULT_SALARIO)
cartera_estado$MONTO_ULT_SALARIO[sample(1:nrow(cartera_estado), 25, FALSE)]
cartera_estado$MONTO_ULT_SALARIO = as.numeric(cartera_estado$MONTO_ULT_SALARIO)
#table(cartera_estado$MONTO_ULT_SALARIO,useNA = "always")
sum(is.na(cartera_estado$MONTO_ULT_SALARIO))
# Hay 55579 NA's
cartera_estado$MONTO_ULT_SALARIO = replace_na(cartera_estado$MONTO_ULT_SALARIO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$MONTO_ULT_SALARIO[cartera_estado$MONTO_ULT_SALARIO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$MONTO_ULT_SALARIO,useNA = "always")


# TIPO   
cartera_estado$TIPO[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$TIPO)
table(cartera_estado$TIPO,useNA = "always")
sum(is.na(cartera_estado$TIPO))
# No hay NA's
cartera_estado$TIPO = replace_na(as.character(cartera_estado$TIPO), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$TIPO[cartera_estado$TIPO == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$TIPO,useNA = "always")


# FECHA_NACIMIENTO
cartera_estado$FECHA_NACIMIENTO[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$FECHA_NACIMIENTO)
cartera_estado$FECHA_NACIMIENTO = as.Date(cartera_estado$FECHA_NACIMIENTO,format="%Y-%m-%d")
#table(cartera_estado$FECHA_NACIMIENTO,useNA = "always")

# Para calcular la edad a partir de la fecha de nacimiento
nacimiento = cartera_estado %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, FECHA_NACIMIENTO)
nacimiento$FECHA_NACIMIENTO = as.numeric(substr(nacimiento$FECHA_NACIMIENTO, 1,4))

hoy = Sys.Date()
hoy = format(hoy, "%Y")
hoy = as.numeric(hoy)

nacimiento = nacimiento %>%
  mutate(EDAD = hoy-nacimiento$FECHA_NACIMIENTO)

table(nacimiento$EDAD,useNA = "always")
sum(is.na(nacimiento$EDAD))
# Hay 19714 NA's
#sort(prop.table(table(nacimiento$EDAD, useNA = "always"))*100,decreasing= T)
# El 5.21% son NA's
nacimiento$EDAD[is.na(nacimiento$EDAD)]<-mfv(nacimiento$EDAD[!is.na(nacimiento$EDAD)])
# En caso de haber NA's, han sido reemplazados con la moda
nacimiento$EDAD[nacimiento$EDAD == ""] <- mfv(nacimiento$EDAD) 
# En caso de haber blancos, habrían sido reemplazados la moda
table(nacimiento$EDAD,useNA = "always")
# Hay varios casos de personas menores de edad y también de personas de 100 años y más, por lo que van a excluirse esos casos.
nacimiento = nacimiento[!(nacimiento$EDAD < 18),]
nacimiento = nacimiento[!(nacimiento$EDAD > 100),]
nacimiento = nacimiento[,-c(4)]
table(nacimiento$EDAD,useNA = "always")
str(nacimiento)

# UBICACIONELECTORAL
cartera_estado$UBICACIONELECTORAL[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$UBICACIONELECTORAL)
#table(cartera_estado$UBICACIONELECTORAL,useNA = "always")
sum(is.na(cartera_estado$UBICACIONELECTORAL))
# Mejor omitirla por el momento.


# SEXO 
cartera_estado$SEXO[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$SEXO)
prop.table(table(cartera_estado$SEXO, useNA = "always"))*100
# El 6.379% de los casos tienen valores NULL o NO INDICA.
table(cartera_estado$SEXO,useNA = "always")
sum(is.na(cartera_estado$SEXO))
# No hay NA's
cartera_estado$SEXO = replace_na(as.character(cartera_estado$SEXO), "NO INDICA")
# En caso de haber NA's, han sido reemplazados con NO INDICA's
cartera_estado$SEXO[cartera_estado$SEXO == "NULL"] <- "NO INDICA" 
# En caso de haber NULL, habrían sido reemplazados con NO INDICA's
table(cartera_estado$SEXO,useNA = "always")


# CANTIDAD_VEHICULOS    
cartera_estado$CANTIDAD_VEHICULOS[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$CANTIDAD_VEHICULOS)
#table(cartera_estado$CANTIDAD_VEHICULOS,useNA = "always")
sum(is.na(cartera_estado$CANTIDAD_VEHICULOS))
# No hay NA's
cartera_estado$CANTIDAD_VEHICULOS = replace_na(cartera_estado$CANTIDAD_VEHICULOS, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_VEHICULOS[cartera_estado$CANTIDAD_VEHICULOS == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_VEHICULOS,useNA = "always")


# CANTIDAD_PROPIEDADES
cartera_estado$CANTIDAD_PROPIEDADES[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$CANTIDAD_PROPIEDADES)
#table(cartera_estado$CANTIDAD_PROPIEDADES,useNA = "always")
sum(is.na(cartera_estado$CANTIDAD_PROPIEDADES))
# No hay NA's
cartera_estado$CANTIDAD_PROPIEDADES = replace_na(cartera_estado$CANTIDAD_PROPIEDADES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROPIEDADES[cartera_estado$CANTIDAD_PROPIEDADES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_PROPIEDADES,useNA = "always")


# CANTIDAD_GESTIONES
cartera_estado$CANTIDAD_GESTIONES[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$CANTIDAD_GESTIONES)
#table(cartera_estado$CANTIDAD_GESTIONES,useNA = "always")
sum(is.na(cartera_estado$CANTIDAD_GESTIONES))
# No hay NA's
cartera_estado$CANTIDAD_GESTIONES = replace_na(cartera_estado$CANTIDAD_GESTIONES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROPIEDADES[cartera_estado$CANTIDAD_GESTIONES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
#table(cartera_estado$CANTIDAD_GESTIONES,useNA = "always")


# ESTADO_CIVIL
cartera_estado$ESTADO_CIVIL[sample(1:nrow(cartera_estado), 25, FALSE)]
str(cartera_estado$ESTADO_CIVIL)
table(cartera_estado$ESTADO_CIVIL,useNA = "always")
prop.table(table(cartera_estado$ESTADO_CIVIL)[order(table(cartera_estado$ESTADO_CIVIL), decreasing = TRUE)])*100

est_civil = c("SOLTERO","MATRIMONIO","DIVORCIO","VIUDEZ")
cartera_estado = cartera_estado %>% 
  mutate(ESTADO_CIVIL = case_when(
    ESTADO_CIVIL %!in% est_civil ~ "DESCONOCIDO",
    ESTADO_CIVIL %in% est_civil ~ ESTADO_CIVIL
  ))

table(cartera_estado$ESTADO_CIVIL,useNA = "always")
sum(is.na(cartera_estado$ESTADO_CIVIL))
# No hay NA's
cartera_estado$ESTADO_CIVIL= replace_na(as.character(cartera_estado$ESTADO_CIVIL), "DESCONOCIDO")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$ESTADO_CIVIL[cartera_estado$ESTADO_CIVIL == "NULL"] <- "DESCONOCIDO" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$ESTADO_CIVIL,useNA = "always")


# Unión de todas las variables anteriores
t(t(names(cartera_estado)))
cartera_estado_depurada = cartera_estado %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, MAO_MONTO_ORIGINAL, MAO_SALDO, EOP_DESCRIPCION,
         PRO_DESCRIPCION, INS_DESCRIPCION, ALQ_DESCRIPCION, TCT_DESCRIPCION, REACCION_OPERACION, REA_DESCRIPCION, 
         REACCION_ULTIMA_GESTION, MONTO_ULTIMO_PAGO, CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS,
         CANTIDAD_COMUNICACIONES, TIENE_EXPEDIENTE, SALDO_CAPITAL, INTERES, CARGOS_PENDIENTES, MONTO_ULT_SALARIO, 
         TIPO, SEXO, CANTIDAD_VEHICULOS, ESTADO_CIVIL, CANTIDAD_GESTIONES) 

# Se une la tabla anterior con nacimiento para tener la edad de los clientes
cartera_estado_depurada = nacimiento %>%
  left_join(cartera_estado_depurada, 
            by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION","CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))

# Se une la tabla anterior con q_operaciones_por_cliente
cartera_estado_depurada = cartera_estado_depurada %>%
  left_join(q_operaciones_por_cliente, by =("CLI_IDENTIFICACION"="CLI_IDENTIFICACION"))
summary(cartera_estado_depurada)
# Al unir estas dos tablas, se ve que hay se ve cómo hay clientes que no pueden ser identificados por ID o por NOMBRE, ya que de ellos sólo se tiene OPE_NUMERO_OPERACION.


# Se une la tabla anterior con la variable respuesta AL MENOS UN PAGO
cartera_estado_depurada = cartera_estado_depurada %>%
  left_join(al_menos_un_pago, 
            by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO",
                   "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
summary(cartera_estado_depurada)
str(cartera_estado_depurada)
t(t(names(cartera_estado_depurada)))


# Se une la tabla anterior con la cartera base. Como cartera_base es la que tiene el filtro de que sólo estamos trabajando con los juicios que están en trámite, hacemos el left_join por esta table para unirle los demás datos que están en la cartera_estado_depurada. 
cartera_final = cartera_base %>%
  left_join(cartera_estado_depurada, 
            by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO",
                   "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))


# Revisión del archivo cartera_final (que pasó de ser la cartera_base con las nuevas variables incluidas)

str(cartera_final)
t(t(names(cartera_final)))

# Verificación de que no hayan NA's
any_na(cartera_final$CLI_IDENTIFICACION)
any_na(cartera_final$CLI_NOMBRE_COMPLETO)
any_na(cartera_final$OPE_NUMERO_OPERACION)
any_na(cartera_final$CANTIDAD_PROCESOS)
any_na(cartera_final$CANTIDAD_HIJOS)
any_na(cartera_final$CANTIDAD_HIJOS_CATEGORIA)
any_na(cartera_final$CANTIDAD_PROPIEDADES)
any_na(cartera_final$SCORE_FINAL)
any_na(cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA)
any_na(cartera_final$EDAD)
cartera_final$EDAD[is.na(cartera_final$EDAD)]<-mfv(cartera_final$EDAD[!is.na(cartera_final$EDAD)])
any_na(cartera_final$MAO_MONTO_ORIGINAL)
cartera_final$MAO_MONTO_ORIGINAL = replace_na(cartera_final$MAO_MONTO_ORIGINAL, 0)
any_na(cartera_final$MAO_SALDO)
cartera_final$MAO_SALDO = replace_na(cartera_final$MAO_SALDO, 0)
any_na(cartera_final$EOP_DESCRIPCION)
cartera_final$EOP_DESCRIPCION= replace_na(as.character(cartera_final$EOP_DESCRIPCION), "DESCONOCIDO")
any_na(cartera_final$PRO_DESCRIPCION)
cartera_final$PRO_DESCRIPCION= replace_na(as.character(cartera_final$PRO_DESCRIPCION), "DESCONOCIDO")
any_na(cartera_final$INS_DESCRIPCION)
cartera_final$INS_DESCRIPCION= replace_na(as.character(cartera_final$INS_DESCRIPCION), "DESCONOCIDO")
any_na(cartera_final$ALQ_DESCRIPCION)
cartera_final$ALQ_DESCRIPCION= replace_na(as.character(cartera_final$ALQ_DESCRIPCION), "DESCONOCIDO")
any_na(cartera_final$TCT_DESCRIPCION)
cartera_final$TCT_DESCRIPCION= replace_na(as.character(cartera_final$TCT_DESCRIPCION), "DESCONOCIDO")
any_na(cartera_final$REACCION_OPERACION)
cartera_final$REACCION_OPERACION= replace_na(as.character(cartera_final$REACCION_OPERACION), "Desconocido")
any_na(cartera_final$REA_DESCRIPCION)
cartera_final$REA_DESCRIPCION= replace_na(as.character(cartera_final$REA_DESCRIPCION), "Desconocido")
any_na(cartera_final$REACCION_ULTIMA_GESTION)
cartera_final$REACCION_ULTIMA_GESTION= replace_na(as.character(cartera_final$REACCION_ULTIMA_GESTION), "Desconocido")
any_na(cartera_final$MONTO_ULTIMO_PAGO)
cartera_final$MONTO_ULTIMO_PAGO = replace_na(cartera_final$MONTO_ULTIMO_PAGO, 0)
any_na(cartera_final$CANTIDAD_PROMESAS_PAGO)
cartera_final$CANTIDAD_PROMESAS_PAGO = replace_na(cartera_final$CANTIDAD_PROMESAS_PAGO, 0)
any_na(cartera_final$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS)
cartera_final$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS = replace_na(cartera_final$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, 0)
any_na(cartera_final$CANTIDAD_COMUNICACIONES)
cartera_final$CANTIDAD_COMUNICACIONES = replace_na(cartera_final$CANTIDAD_COMUNICACIONES, 0)
any_na(cartera_final$TIENE_EXPEDIENTE)
cartera_final$TIENE_EXPEDIENTE= replace_na(as.character(cartera_final$TIENE_EXPEDIENTE), "Desconocido")
any_na(cartera_final$SALDO_CAPITAL)
cartera_final$SALDO_CAPITAL = replace_na(cartera_final$SALDO_CAPITAL, 0)
any_na(cartera_final$INTERES)
cartera_final$INTERES = replace_na(cartera_final$INTERES, 0)
any_na(cartera_final$CARGOS_PENDIENTES)
cartera_final$CARGOS_PENDIENTES = replace_na(cartera_final$CARGOS_PENDIENTES, 0)
any_na(cartera_final$MONTO_ULT_SALARIO)
cartera_final$MONTO_ULT_SALARIO = replace_na(cartera_final$MONTO_ULT_SALARIO, 0)
any_na(cartera_final$TIPO)
cartera_final$TIPO= replace_na(as.character(cartera_final$TIPO), "Desconocido")
any_na(cartera_final$SEXO)
cartera_final$SEXO= replace_na(as.character(cartera_final$SEXO), "NO INDICA")
any_na(cartera_final$CANTIDAD_VEHICULOS)
cartera_final$CANTIDAD_VEHICULOS = replace_na(cartera_final$CANTIDAD_VEHICULOS, 0)
any_na(cartera_final$ESTADO_CIVIL)
cartera_final$ESTADO_CIVIL= replace_na(as.character(cartera_final$ESTADO_CIVIL), "DESCONOCIDO")
any_na(cartera_final$CANTIDAD_GESTIONES)
cartera_final$CANTIDAD_GESTIONES = replace_na(cartera_final$CANTIDAD_GESTIONES, 0)
any_na(cartera_final$CANTIDAD_OPERACIONES_CLIENTE)
cartera_final$CANTIDAD_OPERACIONES_CLIENTE = replace_na(cartera_final$CANTIDAD_OPERACIONES_CLIENTE, 0)
any_na(cartera_final$AL_MENOS_UN_PAGO)
cartera_final$AL_MENOS_UN_PAGO= replace_na(as.character(cartera_final$AL_MENOS_UN_PAGO), "0")

# Las variables categóricas se definen como factores
#cartera_final$ESTADO_JUICIOS_CATEGORIA = as.factor(cartera_final$ESTADO_JUICIOS_CATEGORIA)
cartera_final$CANTIDAD_HIJOS_CATEGORIA = as.factor(cartera_final$CANTIDAD_HIJOS_CATEGORIA)
cartera_final$EOP_DESCRIPCION = as.factor(cartera_final$EOP_DESCRIPCION)
cartera_final$PRO_DESCRIPCION = as.factor(cartera_final$PRO_DESCRIPCION)
cartera_final$INS_DESCRIPCION = as.factor(cartera_final$INS_DESCRIPCION)
cartera_final$ALQ_DESCRIPCION = as.factor(cartera_final$ALQ_DESCRIPCION)
cartera_final$TCT_DESCRIPCION = as.factor(cartera_final$TCT_DESCRIPCION)
cartera_final$REACCION_OPERACION = as.factor(cartera_final$REACCION_OPERACION)
cartera_final$REA_DESCRIPCION = as.factor(cartera_final$REA_DESCRIPCION)
cartera_final$REACCION_ULTIMA_GESTION = as.factor(cartera_final$REACCION_ULTIMA_GESTION)
cartera_final$TIENE_EXPEDIENTE = as.factor(cartera_final$TIENE_EXPEDIENTE)
cartera_final$TIPO = as.factor(cartera_final$TIPO)
cartera_final$SEXO = as.factor(cartera_final$SEXO)
cartera_final$ESTADO_CIVIL = as.factor(cartera_final$ESTADO_CIVIL)

# Para tratar de ajustar, más adelante, un modelo de regresión logística de LASSO (glmnet), la función cv.glmnet (que es la que se va a utilizar) espera que la variable y (AL_MENOS_UN_PAGO) sea de tipo numérico o binario (0/1) en lugar de un factor.
cartera_final$AL_MENOS_UN_PAGO = as.numeric(as.character(cartera_final$AL_MENOS_UN_PAGO))  

# Venía arrastrando la variable ESTADO_JUICIOS_CATEGORIA sólo para asegurarme, de forma adicional, que esa fuera la data que se iba a utilizar. Procedo a eliminarla para continuar con el análisis.
cartera_final = cartera_final[,-c(4)]

# Guardo cartera_final en formato RDS
saveRDS(object = cartera_final, file = "cartera_final.rds")
