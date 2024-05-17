# Trabajo Series de Tiempo
# Serie: Precio de acciones de Netflix
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en dolarés 

# Grupo 2
# Script elborado por Sebastian Gil 


# Librerías y directorio ----
library(readxl)
library(readr)


# Importanción y reconocimiento de la base ----
data <- read_csv("C:\\Users\\HP\\OneDrive\\Documentos\\DOCUMENTOS PERSONAJES\\CESAR\\Series\\Pro_Series\\Series-de-Tiempo\\Datos\\NFLX.csv")

colnames(data)
head(data)

summary(data)

D1 <- data #  Filtral desde el año 2010
summary(D1)

attach(D1)
par(mfrow = c(2,2))
plot(x = Date , y = Open ,type = "l", main = 'Serie de tiempo variable OPEN')
plot(x = Date , y = Close , type = "l", main = 'Serie de tiempo variable CLOSE')
plot(x = Date , y = High , type = "l", main = 'Serie de tiempo variable HIGH')
plot(x = Date , y = Low , type = "l", main = 'Serie de tiempo variable LOW')
par(mfrow = c(1,1))

# Análisis descriptivo y exploratorio de la serie

ts_data <- ts(Close, start = c(2010, 1), frequency = 365)

plot(ts_data, main = "Precio de cierre de acciones de Netflix", ylab = "Precio de cierre", xlab = "Año")


# Análisis de estacionariedad
library(tseries)
adf.test(ts_data, alternative = "stationary")

