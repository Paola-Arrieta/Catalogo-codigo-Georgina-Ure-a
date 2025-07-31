# ---
# title: "Conexión de PostgreSQL con R"
# author: "Georgina Ureña Ballestero"
# date: "2023-09-22"
# output: html_document
# ---
  
# Contraseña para el superusuario base de datos postgres: Gestionadora.

# Material para la instalación de PostgreSQL disponible en https://www.solvetic.com/tutoriales/article/10650-como-instalar-postgresql-en-windows-11/
  
  
# Biblioteca
library(RPostgreSQL)

# Se indica qué sistema manejar se va a utilizar: En este caso es PostgreSQL
drv = dbDriver("PostgreSQL")

# Con el driver anterior se establece la conexión 
conexion = dbConnect(drv, dbname="prediccion_pago", host="localhost", port=5432, user="postgres", password="Gestionadora")


## Buscar en R la versión de esta librería o update

