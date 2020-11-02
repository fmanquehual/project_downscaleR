rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
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

# Funcion de poder


funcion_de_poder <- function(c,d,H){c*(H^d)}

a.i <- sample(seq(5,10, 0.01), 100) ; length(a.i)
H.i <- sample(1:1000, 100); length(H.i)

modelo_de_poder <- nls(a.i~funcion_de_poder(c.i, d.i, H.i))
c <- coefficients(modelo_de_poder)[1]
d <- coefficients(modelo_de_poder)[2]

a.ajustado <- funcion_de_poder(c, d, 1:1000)

plot(a.i~H.i, xlim=c(0, 1000), ylim=c(0, 15))
lines(a.ajustado, col='red', lwd=2)

mean(a.i-a.ajustado)

