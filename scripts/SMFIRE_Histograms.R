######################
# SM_FIRE Histograms #
######################

path.sand <- "/Users/Cori/Desktop/Raw_Data/Dry_Climate_Dry_Soil/"
files.sand <- dir(path.sand)

# Set up some soil variables first
slz <- c(-2.17, -1.50, -1.10, -0.80, -0.60, -0.45, -0.30, -0.20, -0.12, -0.06)

dslz <- vector(length=length(slz)) 
dslz[length(dslz)] <- 0-slz[length(dslz)]

for(s in 1:(length(dslz)-1)){
  dslz[s] <- slz[s+1] - slz[s]    
}
nsoil=which(slz >= -0.20)

source("../ED_Workflow/0_setup/ED_Calcs_Soil_Fire.R") #Source the functions needed to calculate the other soil variables. 
slxsand <- 0.93
slxclay <- 0.033 # I used the soil texture from the original experimental design.

library(ncdf4)
for(i in 1:length(files.sand)){
  sand.test <- nc_open(file.path(path.sand,files.sand[i]))
  soilmoist <- sum(ncvar_get(sand.test, "SoilMoist")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
  soilmoist #Sum of soil moisture for one year? What is this? 
  
  # Calculate sm_fire value from soilmoist
  slmsts <- calc.slmsts(slxsand, slxclay)
  slpots <- calc.slpots(slxsand, slxclay)
  slbs   <- calc.slbs(slxsand, slxclay)
  soilcp <- calc.soilcp(slmsts, slpots, slbs)
  
  sm_fire <- (soilmoist-soilcp)/(slmsts-soilcp)
  sm_fire <- data.frame(sm_fire=sm_fire,
                        year=i)
  
  # Compile into dataframe
  if(i==1){
    smfire_df <- sm_fire
  } else {
    smfire_df <- rbind(smfire_df, sm_fire)
  } # Close data frame loop
  nc_close(sand.test) 
} # Close i loop

# Find 5th percentile and generate histograms
