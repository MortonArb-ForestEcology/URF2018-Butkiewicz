# Unit calculations

# Summon necessary packages
library(ncdf4)

# Summon netcdf files as a test
path.nc <- file.path("../../../Output/extracted_output/")
files.nc <- dir(path.nc)

nc.test <- nc_open(file.path(path.nc,files.nc[1]))
Patch_Area <- nc.test$var$Patch_Area$units
pft <- nc.test$var$PFT_name$units
soil_moist <- nc.test$var$SoilMoist$units
agb <- nc.test$var$Cohort_AbvGrndBiom$units
dbh <- nc.test$var$Cohort_DBH$units
nc_close(nc.test)

# SOILMOIST CALCULATIONS
soilmoist <- nc.test$var$SoilMoist$units 
soilmoist # m3 m-3 I think
dens.h2o <- 1000 # kg m-3
