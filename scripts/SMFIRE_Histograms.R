#####
# Looking at the SoilMoist variables #
#####

# Extract soilmoist from dataset
path.sand <- "/Users/Cori/Desktop/Raw_Data/Dry_Climate_Dry_Soil/"
files.sand <- dir(path.sand)

library(ncdf4)
sand.test <- nc_open(file.path(path.sand,files.sand[1]))
soilmoist <- ncvar_get(sand.test, "SoilMoist")
soilmoist <- data.frame(soilmoist)
nc_close(sand.test)

# Calculate weighted average for top 3 soil layers

slz <- c(-2.17, -1.50, -1.10, -0.80, -0.60, -0.45, -0.30, -0.20, -0.12, -0.06)

dslz <- vector(length=length(slz)) 
dslz[length(dslz)] <- 0-slz[length(dslz)]

for(i in 1:(length(dslz)-1)){
  dslz[i] <- slz[i+1] - slz[i]    
}
nsoil=which(slz >= -0.20)

soil.moist.tmp <- sum(ncdf4::ncvar_get(sand.test, "SoilMoist")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
soil.moist.tmp #Sum of soil moisture for one year? What is this? 

# Calculate sm_fire value from soilmoist

# Find 5th percentile and generate histograms