library(visualizeR)
library(downscaleR)
library(transformeR)
# # devtools::install_github("SantanderMetGroup/climate4R.datasets")
library(climate4R.datasets)
# # devtools::install_github("SantanderMetGroup/calibratoR", force = TRUE)
# library(calibratoR)
# # devtools::install_github("SantanderMetGroup/downscaleR.keras")
# library(downscaleR.keras)
# # devtools::install_github("SantanderMetGroup/VALUE")
# library(VALUE)
# # devtools::install_github(c("SantanderMetGroup/climate4R.climdex"))
# library(climate4R.climdex)
# # install.packages("remotes")
# library(remotes)


data("VALUE_Iberia_tas") # illustrative datasets included in climate4R.datasets
y <- VALUE_Iberia_tas 

data("NCEP_Iberia_hus850", "NCEP_Iberia_psl", "NCEP_Iberia_ta850")
x <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)

# calculating predictors
data <- prepareData(x = x, y = y,spatial.predictors = list(v.exp = 0.95)) 

# Fitting statistical downscaling methods (simple case, no cross-validation)
analog <- downscaleTrain(data, method = "analogs", n.analogs = 1)
regression <- downscaleTrain(data, method = "GLM",family = gaussian)
neuralnet <- downscaleTrain(data, method = "NN", hidden = c(10,5), output = "linear")

# Extracting the results for a particula station (Igueldo) for a single year (2000)
igueldo.2000 <- subsetGrid(y,station.id = "000234",years = 2000)
analog.2000 <- subsetGrid(analog$pred,station.id = "000234",years = 2000)
regression.2000 <- subsetGrid(regression$pred,station.id = "000234",years = 2000)
neuralnet.2000 <- subsetGrid(neuralnet$pred,station.id = "000234",years = 2000)

# plot
temporalPlot(igueldo.2000, analog.2000, regression.2000, neuralnet.2000)
temporalPlot(igueldo.2000, analog.2000, neuralnet.2000)
temporalPlot(regression.2000)

# fuente
# https://github.com/SantanderMetGroup/downscaleR