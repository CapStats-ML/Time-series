# Trabajo Series de Tiempo
# Serie: Precio de acciones del grupo Argos
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en pesos 

# Grupo 2
# Script elborado por Sebastian Gil, Cesar Prieto, Gabriel Peña


# Librerías y directorio ----
library(forecast)
library(MASS)
library(tidyverse)
library(lubridate)
library(timetk)
library(zoo)
library(tsibble)
library(dplyr)
library(feasts)
library(fable)
library(tsibble)
library(astsa)
library(nonlinearTseries)
library(tseriesChaos)
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
par(mfrow = c(1,1))

################################################################################
############ Análisis descriptivo y exploratorio de la serie ###################

Serie <- data[,c(1,2)]
head(Serie)
summary(Serie)

Apertura <- ts(Serie[,2], start = c(2014,11,07), frequency = 7)

attach(Serie)
plot(Apertura, type = "l", main = 'Serie de tiempo OPEN' )

### Estabilizacion de la varianza
lambda_optimo <- forecast::BoxCox.lambda(Apertura, method = "loglik", lower = -2, upper = 2) 
print(lambda_optimo)

# Aplicar Box-Cox solo a la columna 'Último'
Serie$BoxCox <- BoxCox(Apertura, lambda = 1.7)

# Graficar de la comparacion de las series
par(mfrow = c(1,2))
plot(x = Fecha, y = Apertura, type = "l", main = 'Serie de tiempo OPEN' )
plot(x = Fecha, y = BoxCox, type = "l", main = 'Serie de tiempo OPEN-BoxCox')
par(mfrow = c(1,1))

# Coincide con el anterior valor de la varianza
MASS::boxcox(lm(Apertura ~1), seq(0, 4.5, length = 50))
a <- MASS::boxcox(lm(Apertura ~1), seq(0, 4.5, length = 50))

a$x[which.max(a$y)]
plot(a, lambda = a$x[which.max(a$y)])
abline(v = a$x[which.max(a$y)], col= "red")

logApertura <- log(Apertura)

par(mfrow = c(1,2))
plot(x = Fecha, y = logApertura, type = "l", main = 'Serie de tiempo Log-Apertura' )
plot(x = Fecha, y = Apertura, type = "l", main = 'Serie de tiempo Apertura')
par(mfrow = c(1,1))

## Estimación de la tendencia -----
fit_Apertura <- lm (Apertura ~ time(Apertura), na.action = NULL)
summary(fit_Apertura)

# modelo en escala log
fit_logApertura <- lm (logApertura ~ time(logApertura), na.action = NULL)
summary(fit_Apertura)

# Regresión paramétrica no hay tendencia lineal

par(mfrow = c(1, 2))
# Primer panel: Gráfico de la serie original
plot(Serie$Fecha, Apertura, type = "l", ylab = "Valor en escala original")
lines(Serie$Fecha, predict(fit_Apertura), col = "red")


# Segundo panel: Gráfico de la serie en escala logarítmica
plot(Serie$Fecha, logApertura, type = "l", ylab = "Valor en escala log")
lines(Serie$Fecha, predict(fit_logApertura), col = "red")
par(mfrow = c(1, 1))


# Eliminamos la tendencia con la predicción de la recta
# se hace con la diferencia de la tend log, y el mod ajustdo

Apertura.sin.tend <- Apertura- predict(fit_Apertura)

# serie sin tendencia en escala log
logApertura.sin.tend <- logApertura- predict(fit_logApertura)

plot(x= Fecha, y = Apertura.sin.tend, type = "l", main = "Serie Log sin tendencia")
acf(Apertura, lag.max = length(Apertura))
pacf(Apertura, lag.max = length(Apertura))

plot(x = Fecha, y = Apertura.sin.tend, type = "l", main = "Apertura sin tendencia")
acf(Apertura.sin.tend, lag.max = length(Apertura.sin.tend)) 
pacf(Apertura.sin.tend, lag.max = length(Apertura.sin.tend)) 


plot(x= Fecha, y = logApertura.sin.tend, main = "Serie Log sin tendencia")
acf(logApertura, lag.max = length(logApertura))
pacf(logApertura, lag.max = length(logApertura))
acf(logApertura.sin.tend, lag.max = length(logApertura.sin.tend)) 
pacf(logApertura.sin.tend, lag.max = length(logApertura.sin.tend)) 


## Promedio Móvil -----









