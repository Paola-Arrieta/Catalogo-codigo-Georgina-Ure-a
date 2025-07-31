# ---
# title: "Conexión de Mongo DB con R para montos de recuperación; País CR"
# author: "Georgina Ureña Ballestero"
# output: html_document
# date: "2023-11-15"
# ---

# Como solución al almacenamiento de los resultados que vamos obteniendo cada vez que se corren los modelos de series de tiempo para predecir los montos de recuéración, se planteó realizarlo con Mongo DB.
# 
# Mongo DB es un servidor de base de datos, pero se necesita interactuar con dicha base de datos y, para ellos, se tienen dos maneras:
# 1. Por defecto ofrece una consola desde la cual se pueden hacer consultas, crear bases de datos, crear las estructuras o las colecciones, insertar nuevos registros, etc.
# 2. Ofrece una interfaz gráfica para que todo lo anterior sea mucho más fácil; esta es Mongo DB Compass.
# 
# (Tutorial para toda la instalación en https://www.youtube.com/watch?v=-GLMGXkXa7k)
# 
# Ahora bien, para poder guardar las colecciones que se van generando (es decir, los resultados de las predicciones de pago), es necesario conectar R (siendo un lenguaje orientado a objetos y destinado al análisis estadístico y la representación estos datos) a MongoDB Compass. Seguidamente se muestra el código requerido para este procedimiento:
# (Tutorial para consultar disponible en https://www.youtube.com/watch?v=JBEKJflNV2g)
# 
# Antes de realizar el procedimiento en R, se debe "activar" el server. Los pasos son los siguientes:
# 1. Ir a la ubicación de mongo en la computadora (C:\Program Files\MongoDB\Server\7.0\bin).
# 2. Ejecutar mongod.
# 3. En la consola negra, buscar {"address":"127.0.0.1"}} y {"port":27017,"ssl":"off"}} sólo para cerciorarnos de estar en la dirección y el puerto correspondiente.
# 4. Abrir MongoDB Compass.
# 5. Seleccionar la conexión con los detalles anteriores.

# Biblioteca
library(mongolite)

# Llamar la base de datos
predicciones_finales_cr = readRDS(file = "C:/Users/gurenab/Documents/Predicción de recuperación/src/Pais/predicciones_finales_cr.rds")
head(predicciones_finales_cr)

# Conexión con MongoDB collection para agregar la data nueva
c = mongo(collection = "pais_cr", db="Montos_recuperacion")

# Agrego la data nueva
c$insert(predicciones_finales_cr)


# Comprobaciones

# Verifico cúantos de esos valores agregados son distintos
# c$distinct("Fecha")

# Verifico la función aggregate
# c$aggregate()

# Veo cuántos registros tiene esa data
c$count()

# Info de la data agregada en MongoDB
c$info()

# RECORDAR: Una vez finalizado el proceso, en MongoDB Compass ir a Connect y marcar Disconnect para desconectar el servidor.