# Trabajo Series de Tiempo
# Serie: Importaciones Colombianas via maritimas
# Características: 
# - Mediciones Mensuales (Meses)
# - Precio de la carga al momento de ser transportada via maritima

# Grupo 2
# Script elborado por Sebastian Gil, Cesar Prieto, Gabriel Peña



# Librerías y directorio ----
library(readxl)
library(readr)

# Importanción y reconocimiento de la base ----
importaciones <- read.csv("Datos/Importaciones.csv")
names(importaciones)

# Definiciones
# VACIP: Valor CIF pesoso de la mercancia. Valor de las mercancías
#       que incluye el flete hasta el lugar de destino
# VACID: Valor CIF en dólares de la mercancia
# VAFODO: Valor FOB dólares de la mercancía. Valor de la mercancia
#       en el momento que se carga a bordo del medio de transporte 
#       marítimo.
# FLETE: costo a pagar por el desplazamiento de una carga en un medio
#       de transporte.
# IMP1: impuesto a las ventas.
# PBK: peso bruto en kilos.
# PNK: peso neto en kilos.

data$Último <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Último)))
data$Apertura <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Apertura)))
data$Máximo <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Máximo)))
data$Mínimo <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Mínimo)))
v <- data$Vol.
data$Vol. <- as.numeric(gsub(",",".",gsub("K", "", data$Vol.))) * ifelse(grepl("K", v), 1000, 1)
data$X..var. <- as.numeric(gsub("%", "", gsub(",", ".", data$X..var.)))
sum(is.na(data$Vol.))


sum(is.na(importaciones))
a <- c()
for ( i in 3:9){
  a <- c(a, sum(is.na(importaciones[,i])))
}

vacip <- ts(importaciones[,3], start = c(2012, 01), frequency =12)
vafodo <- ts(importaciones[,4], start = c(2012, 01), frequency =12)
flete <- ts(importaciones[,5], start = c(2012, 01), frequency =12)
imp1 <- ts(importaciones[,6], start = c(2012, 01), frequency =12)
vacid <- ts(importaciones[,7], start = c(2012, 01), frequency =12)
pbk <- ts(importaciones[,8], start = c(2012, 01), frequency =12)
pnk <- ts(importaciones[,9], start = c(2012, 01), frequency =12)

plot(vacip)# no 
plot(vafodo) # si
plot(flete) # si 
plot(imp1) # no
plot(vacid) # no 
plot(pbk) # si
plot(pnk) # si


# Librerias ---------------------------------------------------------------

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


# Descriptivo -------------------------------------------------------------
# vafodo
# flete
# pbk
# pnk
serie <- vafodo/1000000000
plot(serie)

## Estabilización de la varianza -----

# Serie estacionaria ???
# Esta función devuelve el valor de lambda
forecast::BoxCox.lambda(serie, method = "loglik", 
                        lower = -2, # limitie menor para el lambda 
                        upper = 2) # limite superior para el lambda
plot(BoxCox(serie, lambda = 0.75))
# NO parece haber mucha diferencia entre ambos graficos
# plot(BoxCox(serie, lambda = 0.75)) y plot(serie)
# podría decirse que la varianza es la misma


# Coincide con el anterior valor de la varianza
MASS::boxcox(lm(serie ~ 1), seq(-2, 7, length = 50))
a <- MASS::boxcox(lm(serie ~ 1), seq(-2, 7, length = 50))

a$x[which.max(a$y)]
abline(v = a$x[which.max(a$y)], col= "red")
?abline()
plot(a, lambda = a$x[which.max(a$y)])

lserie <- log(serie)
plot(lserie)
plot(serie)
## Estimación de la tendencia -----
fit_serie <- lm (serie ~ time(serie), na.action = NULL)
summary(fit_serie)

# modelo en escala log
fit_lserie <- lm (lserie ~ time(serie), na.action = NULL)
summary(fit_lserie)

# Justificar por qué no le quitamos la tendencia
# Regresión paramétrica no hay tendencia lineal


plot(serie, ylab = "Valor/Costo en escala logarítmica")
abline(fit_serie, col = "red") 

# modelo en escala log
plot(lserie, ylab = "Valor/Costo en escala logarítmica")
# añadimos la recta de ajuste
abline(fit_lserie, col = "red") 

# Eliminamos la tendencia con la predicción de la recta
# se hace con la diferencia de la tend log, y el mod ajustdo

serie.sin.tend <- serie- predict(fit_serie)
# serie sin tendencia en escala log
lserie.sin.tend <- lserie- predict(fit_lserie)

plot(serie.sin.tend, main = "Serie Log sin tendencia")
acf(serie, lag.max = length(lserie))
# nos da -indicios- de estacionalidad
acf(serie.sin.tend, lag.max = length(serie.sin.tend)) 


plot(lserie.sin.tend, main = "Serie Log sin tendencia")
acf(lserie, lag.max = length(lserie))
# nos da -indicios- de estacionalidad
acf(lserie.sin.tend, lag.max = length(lserie.sin.tend)) 


## Promedio Móvil -----

descomposicion_serie <- decompose(serie)
plot(descomposicion_serie)

descomposicion_lserie <- decompose(lserie)
plot(descomposicion_lserie)

## Tendencia de STL -----

indice_serie <- as.Date(as.yearmon(tk_index(serie)))
indice_serie1 <- yearmonth(as.yearmon(tk_index(serie)))


indice_logserie <- as.Date(as.yearmon(tk_index(lserie)))
indice_logserie1 <- yearmonth(as.yearmon(tk_index(lserie)))

# Forma alternativa de extraer el indice
df_serie <- data.frame(Fecha = indice_serie, 
                          sserie = as.matrix(serie))
str(df_serie)
tibble_serie <- tibble(df_serie)
tsibble_serie <- as_tsibble(df_serie)

# escala log
df_logserie <- data.frame(Fecha = indice_logserie, 
                              logserie = as.matrix(lserie))
str(df_logserie)
tibble_logserie <- tibble(df_logserie)
tsibble_logserie <- as_tsibble(df_logserie)

# Revisar si hay registros duplicados
# duplicates(tibble_logserie, key = Null, index = Fecha)

# Primera aproximación al ajuste STL 
tsibble_serie %>%
  timetk::plot_time_series(Fecha, sserie,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)
# escala log
tsibble_serie %>%
  timetk::plot_time_series(Fecha, logserie,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)

# Ajuste STL 
tibble_serie %>%  mutate(
  serie_ajust =smooth_vec(sserie, span = 0.75, degree =2)
)

# escala log
tibble_logserie %>%  mutate(
  Logserie_ajust =smooth_vec(logserie, span = 0.75, degree =2)
)

# Ajuste STL moviendo los parámetros
tibble_serie %>% mutate(
  serie_ajus = smooth_vec(sserie, span = 0.9, degree = 2)) %>% 
  ggplot(aes(Fecha, sserie)) + 
  geom_line()+
  geom_line(aes(y = serie_ajus), color = "red")

# escala log
tibble_logserie %>% mutate(
  Logserie_ajus = smooth_vec(logserie, span = 0.9, degree = 2)) %>% 
  ggplot(aes(Fecha, logserie)) + 
  geom_line()+
  geom_line(aes(y = Logserie_ajus), color = "red")

### STL trend y estacionalidad -------------------------------------

tsibble_serie <- as_tsibble(serie)
str(tsibble_serie)
# escala log
tsibble_lserie <- as_tsibble(lserie)
str(tsibble_lserie)

tsibble_serie %>% 
  model(
    STL(value ~ trend() + 
          season(window = "periodic"),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()
# escala log
tsibble_lserie %>% 
  model(
    STL(value ~ trend() + 
          season(window = "periodic"),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()

## Diferencia Ordinaria -------------------------------------------
# Usando diferencia ordinaria
tsibble_serie|>mutate(
  diff_serie = tsibble::difference(value, lag = 1, 
                                    differences = 1))|>
  autoplot(.vars = diff_serie) + 
  labs(subtitle = "Cambio del Costo")
# escala log
tsibble_lserie|>mutate(
  diff_lserie = tsibble::difference(value, lag = 1, 
                                     differences = 1))|>
  autoplot(.vars = diff_lserie) + 
  labs(subtitle = "Cambios en escala logarítmicade del Costo")

tsibble_serie <- tsibble_serie|>mutate(
  diff_serie = tsibble::difference(value, lag = 1,
                                    difference = 1))
# escala log
tsibble_lserie <- tsibble_lserie|>mutate(
  diff_lserie = tsibble::difference(value, lag = 1,
                                      difference = 1))

# Diferenciando basado en el objeto tibble
tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie)) %>% 
  plot_time_series(Fecha, diff_sserie)

tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie)) %>% 
  plot_time_series(Fecha, diff_sserie)

tibble_sserie <- tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie))

# escala log
tibble_logserie %>% 
  mutate(diff_Logserie = logserie - lag(logserie)) %>% 
  plot_time_series(Fecha, diff_Logserie)

tibble_logserie %>% 
  mutate(diff_Logserie = logserie - lag(logserie)) %>% 
  plot_time_series(Fecha, diff_Logserie)

tibble_logserie <- tibble_logserie %>% 
  mutate(diff_logserie = logserie - lag(logserie))

# Diferenciando con base en el objeto ts
dserie <- diff(serie)
plot(dserie)

# escala log
dlserie <- diff(lserie)
plot(dlserie)

## Relaciones no-lineales dispersión ---------------------------------

par(mar = c(3,2,3,2))
astsa::lag1.plot(dserie, 12, corr = T)

#escala log
par(mar = c(3,2,3,2))
astsa::lag1.plot(dlserie, 12, corr = T)

### ACF ---------------------------------------------------------------

acf(dserie, 48, main = "Serie diferenciada de costos")
pacf(dserie, 48)

# escla log
acf(dlserie, 48, main = "Serie diferenciada y con logaritmo de costos")
pacf(dlserie, 48)


## Índice AMI -------------------------------------------------------
# Indice de información mutua
par(mar = c(3,2,3,2))
astsa::lag1.plot(serie, 12, corr = F)
nonlinearTseries::mutualInformation(serie, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)
# escala log
par(mar = c(3,2,3,2))
astsa::lag1.plot(lserie, 12, corr = F)
nonlinearTseries::mutualInformation(lserie, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)
















