library(loadeR)
library(visualizeR)
library(downscaleR)
# nls() # para regresion no lineal


rm(list=ls())
dev.off()

source('funcion_prt.R')

observado <- rnorm(n=65, mean=5, sd=1)
simulados <- observado-1#rnorm(n=65, mean=0, sd=1)

plot(observado, type='l', lwd=2)
lines(simulados, type='l', lwd=1, lty=2, col='red')

correccion <- ptr(o=observado, p=simulados, s=simulados, precip = TRUE)

a <- ptr(o=observado, p=simulados, s=simulados, precip = TRUE, entregar_a = TRUE)
b <- ptr(o=observado, p=simulados, s=simulados, precip = TRUE, entregar_b = TRUE)
correccion2 <- a*(simulados^b)

plot(observado, type='l', lwd=2)
lines(simulados, type='l', lwd=1, lty=2, col='red')
lines(correccion, type='l', lwd=1, lty=2, col='green')
lines(correccion2, type='l', lwd=1, lty=2, col='blue')

