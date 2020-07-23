# bloque 1 ----

C4R <- list("loadeR", "transformeR", "downscaleR", "visualizeR")
lapply(C4R, require, character.only = TRUE)
library(RColorBrewer)
library(climate4R.climdex) #Wrapper for climate indices
library(climate4R.UDG)

lon <- c(-10,20); lat <- c(35,46); seas <- 1:12

eobs <- "http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.25regular/tx_0.25deg_reg_v17.0.nc"
obs.tx <- loadGridData(eobs, var = "tx",
                       years = 1971:2000, season = seas,
                       lonLim = lon, latLim = lat)

obs.SU <- climdexGrid(tx = obs.tx, index.code = "SU")
spatialPlot(climatology(obs.SU))

# fin ---



# bloque 2 ----

cordex <- UDG.datasets(pattern = "EUR44.*historical")$CORDEX
climate4R.UDG::loginUDG(username = "fmanquehual", password = "Francisco&Katherine=2")
rcm.tx <- loadGridData(cordex[5], var = "tasmax",
                       years = 1971:2000, season = seas,
                       lonLim = lon, latLim = lat)
rcm.su <- climdexGrid(tx = rcm.tx, index.code = "SU")
spatialPlot(climatology(rcm.su))

# fin ---




# bloque 3 ----
# how regridding and masking can be easily performed

rcm.SU <- interpGrid(rcm.su, getGrid(obs.SU))
mask <- gridArithmetics(obs.SU, 0, operator = "*")
rcm.SU <- gridArithmetics(rcm.SU, mask, operator = "+")
spatialPlot(climatology(rcm.SU))

# fin ---



# bloque 4 ----

bias <- gridArithmetics(rcm.SU, obs.SU, operator = "-")

b1 <- rev(brewer.pal(n = 9, "PiYG"))
spatialPlot(climatology(bias), at = seq(-100,100,10),
            col.regions = colorRampPalette(b1))
# fin ---



# bloque 5 ----

# Finally (panel e), future projections from the same
# model are loaded from the RCP8.5 scenario (2071-2100)
# and bias adjusted using the biasCorrection function.

# f <- "EUR44.*EC-EARTH.*RCA*RCP85.*RCA4" # original
f <- "EUR44.*EC-EARTH.*RCP85.*RCA4"
fut <- UDG.datasets(pattern = f, full.info = TRUE)$CORDEX
rcp85.tx <- loadGridData(fut$name[1], var = "tasmax",
                         years = 2071:2100, season = seas,
                         lonLim = lon, latLim = lat)

rcp85.su <- climdexGrid(tx = rcp85.tx, index.code = "SU")
rcp85.SU <- interpGrid(rcp85.su, getGrid(obs.SU))

rcp85.bc.tx <- biasCorrection(y = obs.tx, x = rcm.tx,
                              newdata = rcp85.tx, method = "eqm")

rcp85.bc.SU <- climdexGrid(tx = rcp85.bc.tx , index.code = "SU")

temporalPlot("E-OBS" = obs.SU, "SU_hist" = rcm.SU,
             "SU_rcp85" = rcp85.SU, "Adjusted" = rcp85.bc.SU,
             latLim = 41.64, lonLim = -0.89,
             cols = c("black", "red", "red", "blue"))

# fin ---