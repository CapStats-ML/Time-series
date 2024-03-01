# Trabajo Series de Tiempo
# Serie: Precio de acciones del grupo Argos
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en pesos 

# Grupo 2
# Script elborado por Sebastian Gil, Cesar Prieto, Gabriel Peña


# Librerías y directorio ----
library(readxl)
library(readr)

# Importanción y reconocimiento de la base ----
data <- read_delim("Datos/G_ARGOS.csv", delim = ";", escape_double = FALSE, 
                   col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)

colnames(data)
head(data)

summary(data)

attach(data)
par(mfrow = c(2,2))
plot(x = Fecha , y = Apertura ,type = "l", main = 'Serie de tiempo variable OPEN')
plot(x = Fecha , y = Último , type = "l", main = 'Serie de tiempo variable CLOSE')
plot(x = Fecha , y = Máximo , type = "l", main = 'Serie de tiempo variable HIGH')
plot(x = Fecha , y = Mínimo , type = "l", main = 'Serie de tiempo variable LOW')

# Análisis descriptivo y exploratorio de la serie

