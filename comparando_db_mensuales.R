library(lubridate)

# correcto.estaciones <- estaciones
# correcto.era5 <- pr.sum

correcto.estaciones2 <- lista_climate4R_a_db(correcto.estaciones)
correcto.era5.2 <- lista_climate4R_a_db(correcto.era5)
nrow(correcto.estaciones2)
nrow(correcto.era5.2)

correcto.estaciones2$anho <- year(as.Date(correcto.estaciones2$fecha, '%Y-%m-%d'))
correcto.era5.2$anho <- year(as.Date(correcto.era5.2$fecha, '%Y-%m-%d'))

table(correcto.estaciones2$anho)
table(correcto.era5.2$anho)

# ---

# incorrecto.estaciones <- estaciones
# incorrecto.era5 <- pr.sum

incorrecto.estaciones2 <- lista_climate4R_a_db(incorrecto.estaciones)
incorrecto.era5.2 <- lista_climate4R_a_db(incorrecto.era5)
nrow(incorrecto.estaciones2)
nrow(incorrecto.era5.2)

incorrecto.estaciones2$anho <- year(as.Date(incorrecto.estaciones2$fecha, '%Y-%m-%d'))
incorrecto.era5.2$anho <- year(as.Date(incorrecto.era5.2$fecha, '%Y-%m-%d'))

table(incorrecto.estaciones2$anho)
table(incorrecto.era5.2$anho)

