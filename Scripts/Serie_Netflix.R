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


setwd("C:/Users/HP/Downloads")

# Importanción y reconocimiento de la base ----
data <- read_csv("NFLX.csv")

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

# Análisis descriptivo y exploratorio de la serie

#ñalskdjfñalskdjf asdf -asd gaboksjdfhaklsjhdf
# felkajsdlkfj