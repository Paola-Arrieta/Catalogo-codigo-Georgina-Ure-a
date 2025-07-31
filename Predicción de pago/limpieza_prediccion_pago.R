# ---
# title: "Limpieza_prediccion_pago"
# author: "Georgina Ureña Ballestero"
# date: "2023-09-18"
# output: html_document
# ---



# ------------------------------ DEFINICIÓN DEL MEJOR MODELO PARA PREDICCIÓN DE PAGO ------------------------------ #


# ----- BIBLIOTECAS

library(collapse)
library(DBI)
library(dplyr)
library(lubridate)
library(modeest)
library(naniar)
library(odbc)
library(readxl)
library(tidyverse)



# ----- CONEXIÓN CON SQL SERVER PARA CARGA DE LOS DATASETS 

# Se define la conexión
connection <- dbConnect(odbc(),
                        Driver = "SQL Server",
                        Server = "200.74.250.96",
                        Database = "XPERSOFT_BD",
                        UID = "gurena",
                        PWD = "CC#E!rMzuWz3gjw@")



# ----- CARGA DE LOS DATASETS CON LAS VARIABLES QUE SE NECESITAN PARA EL MODELO (ya analizadas y definidas anteriormente)

# Obtengo los datasets con las variables de interés
cartera_estado =  dbGetQuery(connection,"
                     SELECT CLI_IDENTIFICACION AS CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO AS CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION AS OPE_NUMERO_OPERACION,
                            MAO_MONTO_ORIGINAL AS MAO_MONTO_ORIGINAL, MAO_SALDO AS MAO_SALDO, EOP_DESCRIPCION AS EOP_DESCRIPCION,
                            PRO_DESCRIPCION AS PRO_DESCRIPCION, INS_DESCRIPCION AS INS_DESCRIPCION, ALQ_DESCRIPCION AS ALQ_DESCRIPCION,
                            TCT_DESCRIPCION AS TCT_DESCRIPCION, [REACCION OPERACION] AS REACCION_OPERACION, REA_DESCRIPCION AS REA_DESCRIPCION,
                            [REACCION ULTIMA GESTION] AS REACCION_ULTIMA_GESTION, ULTIMO_PAGO AS ULTIMO_PAGO, MONTO_ULTIMO_PAGO AS MONTO_ULTIMO_PAGO,
                            CANTIDAD_PROMESAS_PAGO AS CANTIDAD_PROMESAS_PAGO, CANTIDAD_PROMESAS_PAGO_CUMPLIDAS AS CANTIDAD_PROMESAS_PAGO_CUMPLIDAS,
                            CANTIDAD_COMUNICACIONES AS CANTIDAD_COMUNICACIONES, TIENE_EXPEDIENTE AS TIENE_EXPEDIENTE, SALDO_CAPITAL AS SALDO_CAPITAL,
                            INTERES AS INTERES, CARGOS_PENDIENTES AS CARGOS_PENDIENTES, MONTO_ULT_SALARIO AS MONTO_ULT_SALARIO, TIPO AS TIPO, 
                            FECHA_NACIMIENTO AS FECHA_NACIMIENTO, SEXO AS SEXO, CANTIDAD_VEHICULOS AS CANTIDAD_VEHICULOS,
                            CANTIDAD_PROPIEDADES AS CANTIDAD_PROPIEDADES, ESTADO_CIVIL AS ESTADO_CIVIL, CANTIDAD_GESTIONES AS CANTIDAD_GESTIONES
                     FROM CRED_CARTERA_TOTAL")
hijos = dbGetQuery(connection, "
                   SELECT CLI_IDENTIFICACION AS CLI_IDENTIFICACION, OPE_NUMERO_OPERACION AS OPE_NUMERO_OPERACION, NOMBREHIJO AS NOMBREHIJO
                   FROM CRED_HIJOS")
juicios = dbGetQuery(connection, " 
                     SELECT CLI_IDENTIFICACION AS CLI_IDENTIFICACION, OPE_NUMERO_OPERACION AS OPE_NUMERO_OPERACION, EXPEDIENTE AS EXPEDIENTE, ESTADO AS ESTADO
                     FROM CRED_JUICIOS")
referencias = dbGetQuery(connection, "
                         SELECT CLI_IDENTIFICACION AS CLI_IDENTIFICACION, OPE_NUMERO_OPERACION AS OPE_NUMERO_OPERACION, EMPRESA AS EMPRESA, SALDO AS SALDO, 
                         ESTADO AS ESTADO, DETALLE AS DETALLE
                         FROM CRED_REFERENCIAS")
sociedades = dbGetQuery(connection, "
                        SELECT CLI_IDENTIFICACION AS CLI_IDENTIFICACION, OPE_NUMERO_OPERACION AS OPE_NUMERO_OPERACION, NOMBRE AS NOMBRE, PUESTO AS PUESTO
                        FROM CRED_SOCIEDADES")
# Tanto la tabla PROPIEDADES como VEHICULOS no hacen falta, la data que se necesita se obtiene del dataset cartera_estado.

# Se cierra la conexión
dbDisconnect(connection)



# ----- VERIFICACIÓN DE LOS DATOS DUPLICADOS A NIVEL DE FILA (ES DECIR, DATA COMPLETAMENTE DUPLICADA)

# -- Cartera estado
nrow(cartera_estado) == nrow(distinct(cartera_estado))
# Misma cantidad quiere decir que no hay duplicados

cartera_estado$unificado = paste0(cartera_estado$CLI_IDENTIFICACION, "_", cartera_estado$CLI_NOMBRE_COMPLETO, "_", cartera_estado$OPE_NUMERO_OPERACION, "_",
                                  cartera_estado$MAO_MONTO_ORIGINAL, "_", cartera_estado$MAO_SALDO, "_", cartera_estado$EOP_DESCRIPCION, "_",
                                  cartera_estado$PRO_DESCRIPCION, "_", cartera_estado$INS_DESCRIPCION, "_", cartera_estado$ALQ_DESCRIPCION, "_",
                                  cartera_estado$TCT_DESCRIPCION, "_", cartera_estado$REACCION_OPERACION, "_", cartera_estado$REA_DESCRIPCION, "_",
                                  cartera_estado$REACCION_ULTIMA_GESTION, "_", cartera_estado$ULTIMO_PAGO, "_", cartera_estado$MONTO_ULTIMO_PAGO, "_",
                                  cartera_estado$CANTIDAD_PROMESAS_PAGO, "_", cartera_estado$CANTIDAD_PROMESAS_PAGO_CUMPLIDAS, "_",
                                  cartera_estado$CANTIDAD_COMUNICACIONES, "_", cartera_estado$TIENE_EXPEDIENTE, "_", cartera_estado$SALDO_CAPITAL, "_",
                                  cartera_estado$INTERES, "_", cartera_estado$CARGOS_PENDIENTES, "_", cartera_estado$MONTO_ULT_SALARIO, "_",
                                  cartera_estado$TIPO, "_", cartera_estado$FECHA_NACIMIENTO, "_", cartera_estado$SEXO, "_", cartera_estado$CANTIDAD_VEHICULOS, "_",
                                  cartera_estado$CANTIDAD_PROPIEDADES, "_", cartera_estado$ESTADO_CIVIL, "_", cartera_estado$CANTIDAD_GESTIONES)
cartera_estado$dupli_inicial = duplicated(cartera_estado$unificado)
cartera_estado_duplicados = cartera_estado %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

cartera_estado %>%
  filter(unificado %in% cartera_estado_duplicados) %>%
  arrange(unificado) # %>% View()
# De efectivamente encontrar data duplicada, habría que recurrir a eliminarla.

cartera_estado = cartera_estado %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(cartera_estado$unificado), useNA = "always")
cartera_estado = cartera_estado %>% 
  select(-c(unificado, dupli_inicial))

# -- Hijos
nrow(hijos) == nrow(distinct(hijos))
# Distinta cantidad es que existen duplicados

hijos$unificado = paste0(hijos$CLI_IDENTIFICACION,"_", hijos$OPE_NUMERO_OPERACION,"_", hijos$NOMBREHIJO)
hijos$dupli_inicial = duplicated(hijos$unificado)
hijos_duplicados = hijos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

hijos %>%
  filter(unificado %in% hijos_duplicados) %>%
  arrange(unificado) # %>% View()
# De efectivamente encontrar data duplicada, habría que recurrir a eliminarla.

hijos = hijos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(hijos$unificado), useNA = "always")
hijos = hijos %>% 
  select(-c(unificado, dupli_inicial))

# -- Juicios
nrow(juicios) == nrow(distinct(juicios))
# Distinta cantidad es que existen duplicados

juicios$unificado = paste0(juicios$CLI_IDENTIFICACION, "_", juicios$OPE_NUMERO_OPERACION, "_", juicios$EXPEDIENTE, "_", juicios$ESTADO)
juicios$dupli_inicial = duplicated(juicios$unificado)
juicios_duplicados = juicios %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

juicios %>%
  filter(unificado %in% juicios_duplicados) %>%
  arrange(unificado) # %>% View()
# De efectivamente encontrar data duplicada, habría que recurrir a eliminarla.

juicios = juicios %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(juicios$unificado), useNA = "always")
juicios = juicios %>% 
  select(-c(unificado, dupli_inicial))

# -- Referencias
nrow(referencias) == nrow(distinct(referencias))
#  Distinta cantidad es que existen duplicados

referencias$unificado = paste0(referencias$CLI_IDENTIFICACION, "_", referencias$OPE_NUMERO_OPERACION, "_", referencias$EMPRESA, "_",
                               referencias$SALDO, "_", referencias$ESTADO, "_", referencias$DETALLE)
referencias$dupli_inicial = duplicated(referencias$unificado)
referencias_duplicados = referencias %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

referencias %>% 
  filter(unificado %in% referencias_duplicados) %>%
  arrange(unificado) # %>% View()
# De efectivamente encontrar data duplicada, habría que recurrir a eliminarla.

referencias = referencias %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(referencias$unificado), useNA = "always")
referencias = referencias %>% 
  select(-c(unificado, dupli_inicial))

# -- Sociedades
nrow(sociedades) == nrow(distinct(sociedades))
# Distinta cantidad es que existen duplicados

sociedades$unificado = paste0(sociedades$CLI_IDENTIFICACION, "_", sociedades$OPE_NUMERO_OPERACION, "_", sociedades$NOMBRE, "_",
                              sociedades$PUESTO)
sociedades$dupli_inicial = duplicated(sociedades$unificado)
sociedades_duplicados = sociedades %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

sociedades %>% 
  filter(unificado %in% sociedades_duplicados) %>%
  arrange(unificado)  # %>% View()
# De efectivamente encontrar data duplicada, habría que recurrir a eliminarla.

sociedades = sociedades %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(sociedades$unificado), useNA = "always")
sociedades = sociedades %>% 
  select(-c(unificado, dupli_inicial))



# ----- VERIFICACIÓN DE LOS DATOS DUPLICADOS A NIVEL DE COLUMNA (ES DECIR, DATA REPETIDA EN COLUMNAS)

# -- Cartera estado
head(cartera_estado)

length(cartera_estado$CLI_IDENTIFICACION) # 389668               
length(unique(cartera_estado$CLI_IDENTIFICACION)) # 365282 
# En esta es posible que hayan duplicados, ya que una misma persona puede tener varias deudas.

length(cartera_estado$CLI_NOMBRE_COMPLETO) # 389668             
length(unique(cartera_estado$CLI_NOMBRE_COMPLETO)) # 330288    
# En esta es posible que hayan duplicados, ya que una misma persona puede tener varias deudas o varios deudores tener el mismo nombre.

length(cartera_estado$OPE_NUMERO_OPERACION) # 389668             
length(unique(cartera_estado$OPE_NUMERO_OPERACION)) # 389668  
# Esta sí debe ser única porque el número sí debe estár asociado a una deuda en particular (solamente a una).

# -- Hijos
head(hijos)

length(hijos$CLI_IDENTIFICACION) # 592888                    
length(unique(hijos$CLI_IDENTIFICACION)) # 247516            
# Bajo este contexto sí tiene sentido que el número de operación se repita porque son los hijos por deudor y un deudor puede tener varios hijos.

# -- Juicios
head(juicios)    

length(juicios$CLI_IDENTIFICACION) # 797813
length(unique(juicios$CLI_IDENTIFICACION)) # 229316
# Un mismo cliente puede tener varios procesos (independientemente del estado). 

# -- Referencias
head(referencias)
# Esta data se refiere a deudas en general de la persona.

length(referencias$CLI_IDENTIFICACION) # 498745                    
length(unique(referencias$CLI_IDENTIFICACION)) # 177706
# Un mismo deudor puede tener varias deudas. 

# --- Sociedades
head(sociedades)

length(sociedades$CLI_IDENTIFICACION) # 127204                   
length(unique(sociedades$CLI_IDENTIFICACION)) # 53528
# Un mismo deudor puede pertenecer a varias sociedades. 



# ----- CREACIÓN DE UN ARCHIVO BASE 

# La intención acá es crear un archivo base, el cual sólo contenga las variables de identificación (CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION) e ir agregando las variables que realmente son útiles en el análisis.
cartera_base = cartera_estado %>% 
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION)
dim(cartera_base)  # 389668 x 3
str(cartera_base)

# Me aseguro de que el formato del nombre sea estándar
cartera_base$CLI_NOMBRE_COMPLETO = gsub("\\s+", " ", cartera_base$CLI_NOMBRE_COMPLETO)

# Los NA's se van a reemplazar con Desconocido's
any_na(cartera_base$CLI_IDENTIFICACION)
cartera_base$CLI_IDENTIFICACION = replace_na(as.character(cartera_base$CLI_IDENTIFICACION), "Desconocido")
any_na(cartera_base$CLI_NOMBRE_COMPLETO)
cartera_base$CLI_NOMBRE_COMPLETO = replace_na(as.character(cartera_base$CLI_NOMBRE_COMPLETO), "Desconocido")
any_na(cartera_base$OPE_NUMERO_OPERACION)
cartera_base$OPE_NUMERO_OPERACION = replace_na(as.character(cartera_base$OPE_NUMERO_OPERACION), "Desconocido")



# ----- TRABAJO SOBRE LA DATA DEL ARCHIVO HIJOS

t(t(names(hijos)))

# CLI_IDENTIFICACION
# Cantidad de hijos por deudor
q_hijos = hijos %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_HIJOS=n()) %>% 
  #arrange(desc(CANTIDAD_HIJOS)) %>%
  as.data.frame()
prop.table(table(q_hijos$CANTIDAD_HIJOS))*100



# ----- REVISIÓN DE LA CARTERA BASE

# Se unen las tablas anteriores
cartera_base = cartera_base %>% 
  left_join(q_hijos, by=c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
nrow(cartera_base) # 389668

# Los NA's se van a reemplazar con 0's
table(cartera_base$CANTIDAD_HIJOS)
cartera_base$CANTIDAD_HIJOS = replace_na(cartera_base$CANTIDAD_HIJOS, 0)
table(cartera_base$CANTIDAD_HIJOS)

# Se van a plantear 2 variables para la cantidad de hijos: Una que consiste en el número de hijos tal como lo indica el dataset de hijos (2 hijos para el CLI_IDENTIFICACION CR-P-1-1119-0278-SB, por ejemplo) y otra que indica rangos.

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
# Hay 9.11% con 4 hijos o más.

cartera_base$CANTIDAD_HIJOS_CATEGORIA = factor(cartera_base$CANTIDAD_HIJOS_CATEGORIA,
                                               order = TRUE,
                                               levels =c("Sin hijos", "1 hijo", "2 hijos", "3 hijos","4 hijos o más"))
str(cartera_base)
nrow(cartera_base)  # 389668



# ----- TRABAJO SOBRE LA BASE DEL ARCHIVO JUICIOS

# Revisando el archivo
str(juicios)
t(t(names(juicios)))

# CLI_IDENTIFICACION
length(juicios$CLI_IDENTIFICACION) # 797813
length(unique(juicios$CLI_IDENTIFICACION)) # 229316

# Cantidad de veces que se repite cada operación
q_juicios = juicios %>% 
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_PROCESOS=n()) %>% 
  #arrange(desc(CANTIDAD_PROCESOS)) %>%
  as.data.frame()

table(q_juicios$CANTIDAD_PROCESOS)
prop.table(table(q_juicios$CANTIDAD_PROCESOS))*100

# EXPEDIENTE
juicios$EXPEDIENTE[sample(1:nrow(juicios),10)]
# El número de expediente "18-015865-1170-CJ" se interpreta como el expediente # 15865 del Juzgado 1170 de cobro judicial y demandado en el 2018. Además, entre CI (juzgado civil) y CJ (juzgado judicial) no parece existir una diferencia relevante. Es decir, el orden es: Año, número de expediente, número de juzgado y tipo de juzgado.  
# Por lo anterior, se excluye del análisis el número de expediente, sólo se utilizó para asegurarnos de tener registros únicos.

# DETALLE
# Se excluye del análisis el detalle, sólo se utilizó para asegurarnos de tener registros únicos.

# ESTADO
juicios$ESTADO[sample(1:nrow(juicios),10)]
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
# Conversando con Don Willie, se debe filtrar por aquellos casos que se encuentran en trámite. Por lo que se realiza esa acción para continuar con el análisis.

juicios_en_tramite = juicios %>%
  filter(ESTADO_JUICIOS_CATEGORIA == "En tramite") %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, ESTADO_JUICIOS_CATEGORIA) %>%
  summarise(CANTIDAD_PROCESOS=n()) 
# Nos quedamos, entonces, con los clientes que tienen juicios en la categoría "En trámite" y con la cantidad de procesos en ese estado.

# Se une lo anterior con la cartera_base
cartera_base = juicios_en_tramite %>%
  left_join(cartera_base, by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
nrow(cartera_base)  # 217724
str(cartera_base)
cartera_base = as.data.frame(cartera_base %>%
                               select (CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, ESTADO_JUICIOS_CATEGORIA, CANTIDAD_PROCESOS, CANTIDAD_HIJOS, CANTIDAD_HIJOS_CATEGORIA))

table(cartera_base$ESTADO_JUICIOS_CATEGORIA, useNA = "always")
# Hay 217724 procesos en trámite. 
nrow(cartera_base) # 217724



# ----- REVISIÓN DE LA CARTERA BASE

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

nrow(cartera_base) # 217724
nrow(na.omit(cartera_base))==nrow(cartera_base)
# Si es la misma cantidad quiere decir que no hay duplicados.



# ----- TRABAJO SOBRE LA DATA DE REFERENCIAS

# Las referencias son básicamente deudas
dim(referencias) # 498745 x 6
t(t(names(referencias)))
str(referencias)

# EMPRESA  
# Se excluye del análisis la empresa, sólo se utilizó para asegurarnos de tener registros únicos.

# SALDO
# Debido a los problemas que está generando esta variable por los distintos formatos que posee, se decide avanzar sin ella hasta que Don Willie dé una indicación al respecto. Cabe destacar que sólo se utilizó para asegurarnos de tener registros únicos.

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

## - Creo un tipo "score" que tome como puntos positivos la cantidad de referencias que indiquen pago o señal de este y los "sume" en una puntuación final, y que tome como puntos negativos la cantidad de referencias que no indiquen pago o incumplimiento de este y los "reste" de la puntuación final. --- ##

# Puntos positivos
puntos_positivos = q_referencias_estado %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>%
  filter(REFERENCIA_ESTADO_AGRUPADO=='Cancelado' | REFERENCIA_ESTADO_AGRUPADO=='Arreglo' | REFERENCIA_ESTADO_AGRUPADO=='Aldia') %>%
  mutate(SCORE_POSITIVO = sum(CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO)) %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, SCORE_POSITIVO) 

nrow(puntos_positivos) == nrow(distinct(puntos_positivos))
# Distinta cantidad (5895 y 5787, respectivamente); o sea, existen duplicados

puntos_positivos$unificado = paste0(puntos_positivos$CLI_IDENTIFICACION, "_", puntos_positivos$OPE_NUMERO_OPERACION , "_",
                                    puntos_positivos$SCORE_POSITIVO)
puntos_positivos$dupli_inicial = duplicated(puntos_positivos$unificado)
puntos_positivos_duplicados = puntos_positivos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

puntos_positivos %>% 
  filter(unificado %in% puntos_positivos_duplicados) %>%
  arrange(unificado) # %>% View()
# Si efectivamente existe data duplicada, hay que recurrir a eliminarla.

puntos_positivos = puntos_positivos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(puntos_positivos$unificado), useNA = "always")
puntos_positivos = puntos_positivos %>% 
  select(-c(unificado, dupli_inicial))

# Puntos negativos
puntos_negativos = q_referencias_estado %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>%
  filter(REFERENCIA_ESTADO_AGRUPADO=='Administrativo' | REFERENCIA_ESTADO_AGRUPADO=='Legal' | REFERENCIA_ESTADO_AGRUPADO=='Incobrable' |
           REFERENCIA_ESTADO_AGRUPADO=='Judicial' | REFERENCIA_ESTADO_AGRUPADO=='Mora' | REFERENCIA_ESTADO_AGRUPADO=='Otros' |
           REFERENCIA_ESTADO_AGRUPADO=='Desconocido' | REFERENCIA_ESTADO_AGRUPADO=='Revisar') %>%
  mutate(SCORE_NEGATIVO = sum(CANTIDAD_REFERENCIAS_ESTADO_AGRUPADO)) %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, SCORE_NEGATIVO)

nrow(puntos_negativos) == nrow(distinct(puntos_negativos))
# Si hay distinta cantidad quiere decir que existen duplicados

puntos_negativos$unificado = paste0(puntos_negativos$CLI_IDENTIFICACION, "_", puntos_negativos$OPE_NUMERO_OPERACION , "_",
                                    puntos_negativos$SCORE_NEGATIVO)
puntos_negativos$dupli_inicial = duplicated(puntos_negativos$unificado)
puntos_negativos_duplicados = puntos_negativos %>%
  filter(dupli_inicial == TRUE) %>%
  select(unificado) %>%
  unlist() %>%
  as.character()

puntos_negativos %>% 
  filter(unificado %in% puntos_negativos_duplicados) %>%
  arrange(unificado) # %>% View()
# Si efectivamente existe data duplicada, hay que recurrir a eliminarla.

puntos_negativos = puntos_negativos %>% 
  filter(dupli_inicial == FALSE)
table(duplicated(puntos_negativos$unificado), useNA = "always")
puntos_negativos = puntos_negativos %>% 
  select(-c(unificado, dupli_inicial))

# Uno ambas tablas
puntos = puntos_positivos %>%
  full_join(puntos_negativos, by = c('CLI_IDENTIFICACION'='CLI_IDENTIFICACION', 'OPE_NUMERO_OPERACION'='OPE_NUMERO_OPERACION'))

any_na(puntos$SCORE_POSITIVO)
puntos$SCORE_POSITIVO = replace_na(puntos$SCORE_POSITIVO, 0)
# En caso de haber NA's en SCORE_positive, han sido reemplazados con 0's

any_na(puntos$SCORE_NEGATIVO)
puntos$SCORE_NEGATIVO = replace_na(puntos$SCORE_NEGATIVO, 0)
# En caso de haber NA's en SCORE_negative, han sido reemplazados con 0's  

# Se calcula la puntuación final           
puntos_final = puntos %>%
  mutate(SCORE_FINAL = SCORE_POSITIVO-SCORE_NEGATIVO)

nrow(puntos_final) == nrow(distinct(puntos_final))
# Si la cantidad es la misma, quiere decir que no existen duplicados.      

# La tabla anterior se reduce a la puntuación final.
puntos_final = puntos_final %>%
  select(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION, SCORE_FINAL)
nrow(puntos_final) # 186664

## - Fin del cálculo del "score" planteado.

# DETALLE
# Se excluye del análisis la empresa, sólo se utilizó para asegurarnos de tener registros únicos.

# Se une lo anterior con la base para la cartera que se está formando
cartera_base = cartera_base %>%
  left_join(puntos_final, by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))
t(t(names(cartera_base)))



# ----- REVISIÓN DE LA CARTERA BASE

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))

# SCORE_FINAL
table(cartera_base$SCORE_FINAL,useNA = "always")
sum(is.na(cartera_base$SCORE_FINAL))
# Hay 77528 NA's
cartera_base$SCORE_FINAL = replace_na(cartera_base$SCORE_FINAL, 0)
# En caso de haber NA's, han sido reemplazados con 0's
table(cartera_base$SCORE_FINAL,useNA = "always")



# ----- TRABAJO SOBRE LA DATA DE SOCIEDADES

dim(sociedades) # 127204 x 4  
str(sociedades)
t(t(names(sociedades)))

# En cuántas sociedades tiene algún tipo de participación
q_sociedades = sociedades %>%
  group_by(CLI_IDENTIFICACION, OPE_NUMERO_OPERACION) %>% 
  summarise(CANTIDAD_SOCIEDADES_PARTICIPA = n())

# Uno la tabla anterior con la cartera_base
nrow(q_sociedades) # 58655
nrow(cartera_base) # 217724
cartera_base = cartera_base %>%
  left_join(q_sociedades, by=c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))



# ----- REVISIÓN DE LA CARTERA BASE

str(cartera_base)
cartera_base = as.data.frame(cartera_base)
t(t(names(cartera_base)))

# CANTIDAD_SOCIEDADES_PARTICIPA
table(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA,useNA = "always")
sum(is.na(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA))
# Hay 181007 NA's
cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA = replace_na(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA[cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_base$CANTIDAD_SOCIEDADES_PARTICIPA,useNA = "always")



# ----- TRABAJO SOBRE LA DATA DEL ARCHIVO CARTERA_ESTADO

# -- Cartera estado

# Tamaño del archivo
dim(cartera_estado) # 389668 x 30
str(cartera_estado)

# CLI_IDENTIFICACION 
str(cartera_estado$CLI_IDENTIFICACION)
# Comparando tamaño con o sin duplicados en CLI_IDENTIFICACION
length(cartera_estado$CLI_IDENTIFICACION) # 389668
length(unique(cartera_estado$CLI_IDENTIFICACION)) # 365282

# CLI_NOMBRE_COMPLETO
str(cartera_estado$CLI_NOMBRE_COMPLETO)

# OPE_NUMERO_OPERACION 
str(cartera_estado$OPE_NUMERO_OPERACION)

# Creando variable para cantidad de operaciones por identificación
# Ya se vio que existe diferencia entre identificaciones e identificaciones únicas. Esto se debe a que hay deudores que tienen más de una deuda.
# Se va a crear la variable q_operaciones_por_cliente para saber la cantidad de operaciones por cliente para tener en cuenta que, por ejemplo, una persona puede tener varias deudas sobre las cuales GC tiene los derechos.

q_operaciones_por_cliente = cartera_estado %>%
  group_by(CLI_IDENTIFICACION) %>%
  select(OPE_NUMERO_OPERACION) %>%
  summarise(CANTIDAD_OPERACIONES_CLIENTE = n()) %>%
  as.data.frame()

# MAO_MONTO_ORIGINAL
str(cartera_estado$MAO_MONTO_ORIGINAL)
summary(cartera_estado$MAO_MONTO_ORIGINAL)
sum(is.na(cartera_estado$MAO_MONTO_ORIGINAL))
# No hay NA's, pero se denota que existen los valores negativos e iguales a cero
cartera_estado %>%
  filter(cartera_estado$MAO_MONTO_ORIGINAL<=0) # %>% View() # Son 401 casos.
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero. 
cartera_estado = cartera_estado %>% 
  mutate(MAO_MONTO_ORIGINAL = replace(MAO_MONTO_ORIGINAL, MAO_MONTO_ORIGINAL<0, 0))
cartera_estado$MAO_MONTO_ORIGINAL[cartera_estado$MAO_MONTO_ORIGINAL == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# MAO_SALDO 
str(cartera_estado$MAO_SALDO)
cor(cartera_estado$MAO_MONTO_ORIGINAL, cartera_estado$MAO_SALDO)
# Ambas variables aumentan; es decir, entre más alto el monto original, más alto el saldo.
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero.
cartera_estado = cartera_estado %>% 
  mutate(MAO_SALDO = replace(MAO_SALDO, MAO_SALDO<0, 0))
cartera_estado$MAO_SALDO[cartera_estado$MAO_SALDO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# EOP_DESCRIPCION
str(cartera_estado$EOP_DESCRIPCION)
#cartera_estado$EOP_DESCRIPCION
sort(table(cartera_estado$EOP_DESCRIPCION, useNA = "always"),decreasing= T)
sort(prop.table(table(cartera_estado$EOP_DESCRIPCION, useNA = "always"))*100,decreasing= T)
# El 80.65% corresponden a operaciones Inactivas, el 11.90% a Canceladas, el 4.97% a Activas y el 2.32% a Ilocalizable. Qué significa "NULL" y "excluidas"?
table(cartera_estado$EOP_DESCRIPCION,useNA = "always")
sum(is.na(cartera_estado$EOP_DESCRIPCION))
# No hay NA's
cartera_estado$EOP_DESCRIPCION = replace_na(as.character(cartera_estado$EOP_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$EOP_DESCRIPCION[cartera_estado$EOP_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$EOP_DESCRIPCION,useNA = "always")

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
sum(is.na(cartera_estado$ALQ_DESCRIPCION))
# No hay NA's
cartera_estado$ALQ_DESCRIPCION = replace_na(as.character(cartera_estado$ALQ_DESCRIPCION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$ALQ_DESCRIPCION[cartera_estado$ALQ_DESCRIPCION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
#table(cartera_estado$ALQ_DESCRIPCION,useNA = "always")

# TCT_DESCRIPCION
str(cartera_estado$TCT_DESCRIPCION)
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
sum(is.na(cartera_estado$REACCION_ULTIMA_GESTION))
# No hay NA's
cartera_estado$REACCION_ULTIMA_GESTION = replace_na(as.character(cartera_estado$REACCION_ULTIMA_GESTION), "Desconocido")
# En caso de haber NA's, han sido reemplazados con Desconocidos's
cartera_estado$REACCION_ULTIMA_GESTION[cartera_estado$REACCION_ULTIMA_GESTION == "NULL"] <- "Desconocido" 
# En caso de haber NULL, habrían sido reemplazados con Desconocido's
table(cartera_estado$REACCION_ULTIMA_GESTION,useNA = "always")

# ULTIMO_PAGO
str(cartera_estado$ULTIMO_PAGO)
# No se va a incluir como tal, si no que se va a trabajar para que sea la variable respuesta.

## - Creación de la variable respuesta:  

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
al_menos_un_pago = al_menos_un_pago %>%
  select(CLI_IDENTIFICACION, CLI_NOMBRE_COMPLETO, OPE_NUMERO_OPERACION, AL_MENOS_UN_PAGO)

## - Fin de la creación de la variable respuesta. 

# MONTO_ULTIMO_PAGO
str(cartera_estado$MONTO_ULTIMO_PAGO)
cartera_estado$MONTO_ULTIMO_PAGO = as.numeric(cartera_estado$MONTO_ULTIMO_PAGO)
summary(cartera_estado$MONTO_ULTIMO_PAGO)
sum(is.na(cartera_estado$MONTO_ULTIMO_PAGO))
sum(is.na(cartera_estado$MONTO_ULTIMO_PAGO))
# Hay 297928 NA's
cartera_estado$MONTO_ULTIMO_PAGO = replace_na(cartera_estado$MONTO_ULTIMO_PAGO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$MONTO_ULTIMO_PAGO[cartera_estado$MONTO_ULTIMO_PAGO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# CANTIDAD_PROMESAS_PAGO
str(cartera_estado$CANTIDAD_PROMESAS_PAGO)
sum(is.na(cartera_estado$CANTIDAD_PROMESAS_PAGO))
# No hay NA's
cartera_estado$CANTIDAD_PROMESAS_PAGO = replace_na(cartera_estado$CANTIDAD_PROMESAS_PAGO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROMESAS_PAGO[cartera_estado$CANTIDAD_PROMESAS_PAGO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

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

# SALDO_CAPITAL 
str(cartera_estado$SALDO_CAPITAL)
cartera_estado$SALDO_CAPITAL = sub(",", ".", cartera_estado$SALDO_CAPITAL, fixed = TRUE)
cartera_estado$SALDO_CAPITAL = as.numeric(cartera_estado$SALDO_CAPITAL)
sum(is.na(cartera_estado$SALDO_CAPITAL))
# No hay NA's
cartera_estado$SALDO_CAPITAL = replace_na(cartera_estado$SALDO_CAPITAL, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$SALDO_CAPITAL[cartera_estado$SALDO_CAPITAL == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
# Puesto que en el contexto un valor negativo no tiene sentido, en estor casos se va a colocar un cero. 
cartera_estado = cartera_estado %>% 
  mutate(SALDO_CAPITAL = replace(SALDO_CAPITAL, SALDO_CAPITAL<0, 0))

# INTERES 
str(cartera_estado$INTERES)
cartera_estado$INTERES = sub(",", ".", cartera_estado$INTERES, fixed = TRUE)
cartera_estado$INTERES = as.numeric(cartera_estado$INTERES)
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

# CARGOS_PENDIENTES    
str(cartera_estado$CARGOS_PENDIENTES)
cartera_estado$CARGOS_PENDIENTES = sub(",", ".", cartera_estado$CARGOS_PENDIENTES, fixed = TRUE)
cartera_estado$CARGOS_PENDIENTES = as.numeric(cartera_estado$CARGOS_PENDIENTES)
sum(is.na(cartera_estado$CARGOS_PENDIENTES))
# No hay NA's
cartera_estado$CARGOS_PENDIENTES = replace_na(cartera_estado$CARGOS_PENDIENTES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CARGOS_PENDIENTES[cartera_estado$CARGOS_PENDIENTES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# MONTO_ULT_SALARIO
str(cartera_estado$MONTO_ULT_SALARIO)
cartera_estado$MONTO_ULT_SALARIO = as.numeric(cartera_estado$MONTO_ULT_SALARIO)
sum(is.na(cartera_estado$MONTO_ULT_SALARIO))
# Hay 55579 NA's
cartera_estado$MONTO_ULT_SALARIO = replace_na(cartera_estado$MONTO_ULT_SALARIO, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$MONTO_ULT_SALARIO[cartera_estado$MONTO_ULT_SALARIO == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# TIPO   
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
# Esta variable sólo se va a utilizar para calcular la edad de los deudores
str(cartera_estado$FECHA_NACIMIENTO)
cartera_estado$FECHA_NACIMIENTO = as.Date(cartera_estado$FECHA_NACIMIENTO,format="%Y-%m-%d")

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
# Hay 21520 NA's
# El 5.21% son NA's
nacimiento$EDAD[is.na(nacimiento$EDAD)] <- mfv(nacimiento$EDAD[!is.na(nacimiento$EDAD)])
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

# SEXO 
str(cartera_estado$SEXO)
prop.table(table(cartera_estado$SEXO, useNA = "always"))*100
# El 6.74% de los casos tienen valores NULL o NO INDICA.
table(cartera_estado$SEXO,useNA = "always")
sum(is.na(cartera_estado$SEXO))
# No hay NA's
cartera_estado$SEXO = replace_na(as.character(cartera_estado$SEXO), "NO INDICA")
# En caso de haber NA's, han sido reemplazados con NO INDICA's
cartera_estado$SEXO[cartera_estado$SEXO == "NULL"] <- "NO INDICA" 
# En caso de haber NULL, habrían sido reemplazados con NO INDICA's
table(cartera_estado$SEXO,useNA = "always")

# CANTIDAD_VEHICULOS    
str(cartera_estado$CANTIDAD_VEHICULOS)
sum(is.na(cartera_estado$CANTIDAD_VEHICULOS))
# No hay NA's
cartera_estado$CANTIDAD_VEHICULOS = replace_na(cartera_estado$CANTIDAD_VEHICULOS, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_VEHICULOS[cartera_estado$CANTIDAD_VEHICULOS == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_VEHICULOS,useNA = "always")

# CANTIDAD_PROPIEDADES
str(cartera_estado$CANTIDAD_PROPIEDADES)
sum(is.na(cartera_estado$CANTIDAD_PROPIEDADES))
# No hay NA's
cartera_estado$CANTIDAD_PROPIEDADES = replace_na(cartera_estado$CANTIDAD_PROPIEDADES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROPIEDADES[cartera_estado$CANTIDAD_PROPIEDADES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's
table(cartera_estado$CANTIDAD_PROPIEDADES,useNA = "always")

# CANTIDAD_GESTIONES
str(cartera_estado$CANTIDAD_GESTIONES)
sum(is.na(cartera_estado$CANTIDAD_GESTIONES))
# No hay NA's
cartera_estado$CANTIDAD_GESTIONES = replace_na(cartera_estado$CANTIDAD_GESTIONES, 0)
# En caso de haber NA's, han sido reemplazados con 0's
cartera_estado$CANTIDAD_PROPIEDADES[cartera_estado$CANTIDAD_GESTIONES == ""] <- 0 
# En caso de haber blancos, habrían sido reemplazados con 0's

# ESTADO_CIVIL
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
         TIPO, SEXO, CANTIDAD_VEHICULOS, CANTIDAD_PROPIEDADES, ESTADO_CIVIL, CANTIDAD_GESTIONES) 

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

# Se une la tabla anterior con la cartera base. Como cartera_base es la que tiene el filtro de que sólo estamos trabajando con los juicios que están en trámite, hacemos el left_join por esta tabla para unirle los demás datos que están en la cartera_estado_depurada. 
cartera_final = cartera_base %>%
  left_join(cartera_estado_depurada, 
            by = c("CLI_IDENTIFICACION"="CLI_IDENTIFICACION", "CLI_NOMBRE_COMPLETO"="CLI_NOMBRE_COMPLETO",
                   "OPE_NUMERO_OPERACION"="OPE_NUMERO_OPERACION"))



# ----- REVISIÓN DEL ARCHIVO CARTERA_FINAL (QUE PASÓ DE SER LA CARTERA_BASE CON LAS NUEVAS VARIABLES INCLUIDAS)

str(cartera_final)
t(t(names(cartera_final)))

# Verificación de que no hayan NA's
any_na(cartera_final$CLI_IDENTIFICACION)
cartera_final$CLI_IDENTIFICACION = replace_na(as.character(cartera_final$CLI_IDENTIFICACION), "Desconocido")
any_na(cartera_final$CLI_NOMBRE_COMPLETO)
cartera_final$CLI_NOMBRE_COMPLETO = replace_na(as.character(cartera_final$CLI_NOMBRE_COMPLETO), "Desconocido")
any_na(cartera_final$OPE_NUMERO_OPERACION)
cartera_final$OPE_NUMERO_OPERACION = replace_na(as.character(cartera_final$OPE_NUMERO_OPERACION), "Desconocido")
any_na(cartera_final$CANTIDAD_PROCESOS)
cartera_final$CANTIDAD_PROCESOS = replace_na(cartera_final$CANTIDAD_PROCESOS, 0)
any_na(cartera_final$CANTIDAD_HIJOS)
cartera_final$CANTIDAD_HIJOS = replace_na(cartera_final$CANTIDAD_HIJOS, 0)
any_na(cartera_final$CANTIDAD_HIJOS_CATEGORIA)
cartera_final$CANTIDAD_HIJOS_CATEGORIA = replace_na(as.character(cartera_final$CANTIDAD_HIJOS_CATEGORIA), "Desconocido")
any_na(cartera_final$CANTIDAD_PROPIEDADES)
cartera_final$CANTIDAD_PROPIEDADES = replace_na(cartera_final$CANTIDAD_PROPIEDADES, 0)
any_na(cartera_final$SCORE_FINAL)
cartera_final$SCORE_FINAL = replace_na(cartera_final$SCORE_FINAL, 0)
any_na(cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA)
cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA = replace_na(cartera_final$CANTIDAD_SOCIEDADES_PARTICIPA, 0)
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
cartera_final = cartera_final %>%
  select(-c(ESTADO_JUICIOS_CATEGORIA))

# Guardo cartera_final en formato RDS
saveRDS(object = cartera_final, file = "cartera_final.rds")
