######################
# SM_FIRE Histograms #
######################

setwd("/Users/Cori/Desktop/GitHub/URF2018-Butkiewicz/scripts/")
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
  # soilmoist <- sum(ncvar_get(sand.test, "SoilMoist")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
  soilmoist <- data.frame(ncvar_get(sand.test, "SoilMoist"))
  soilmoist <- soilmoist[nsoil,]
  soilmoist <- colMeans(soilmoist) # Not a weighted average, but the best that I could do. 
  soilmoist <- min(soilmoist) # I took the minimum value from the year because I just want to know how many years a fire occurs, not how many times it catches fire that year. This is as good an approximation as any, if you ask me.  
  soilmoist #Sum of soil moisture for one year? What is this? 
  
  # Calculate sm_fire value from soilmoist
  slmsts <- calc.slmsts(slxsand, slxclay)
  slpots <- calc.slpots(slxsand, slxclay)
  slbs   <- calc.slbs(slxsand, slxclay)
  soilcp <- calc.soilcp(slmsts, slpots, slbs)
  
  sm_fire <- (soilmoist-soilcp)/(slmsts-soilcp)
  sm_fire <- data.frame(sm_fire)
  
  # Compile into dataframe
  if(i==1){
    smfire_df <- sm_fire
  } else {
    smfire_df <- rbind(smfire_df, sm_fire)
  } # Close data frame loop
  nc_close(sand.test) 
} # Close i loop

# Find threshold for fire to occur every 20 years. 
FRI_20 <- quantile(smfire_df$sm_fire, 0.05) 
FRI_20

# Find threshold for fire to occur every 5 years. 
FRI_5 <- quantile(smfire_df$sm_fire, 0.20)
FRI_5 #This returns a value around 0.02, which is what I had set for the low fire return interval in my dry climates.

#Generate a histogram
hist(smfire_df$sm_fire,
     col="orange",
     xlab="SM_FIRE",
     main="Dry Climate and Sandy Soil")

library(MASS)
# I want a table with predicted sm_fire values.
sm_fire <- c(0, 0.01, 0.015, 0.02, 0.025, 0.03)
s=0
for (s in sm_fire){
  smfire_subset <- smfire_df[which(smfire_df$sm_fire <= s),]
  FRI <- as.numeric(1/(fractions(length(smfire_subset)/length(smfire_df$sm_fire))))
  FRI <- data.frame(FRI=FRI,
                    sm_fire=s)
  if(s==0){
    FRI_df <- data.frame(FRI)
  } else {
    FRI_df <- rbind(FRI_df, FRI)
  }
}

#########
# I don't have the time to figure out how to loop this properly so I'll just tack the Clay histograms onto the end until I can figure something out
########

path.clay <- "/Users/Cori/Desktop/Raw_Data/Dry_Climate_Wet_Soil/"
files.clay <- dir(path.clay)

slxsand <- 0.38
slxclay <- 0.25
for(i in 1:length(files.clay)){
  clay.test <- nc_open(file.path(path.clay,files.clay[i]))
  # soilmoist <- sum(ncvar_get(clay.test, "SoilMoist")[nsoil]*dslz[nsoil]/sum(dslz[nsoil]))
  soilmoist <- data.frame(ncvar_get(clay.test, "SoilMoist"))
  soilmoist <- soilmoist[nsoil,]
  soilmoist <- colMeans(soilmoist) # Not a weighted average, but the best that I could do. 
  soilmoist <- min(soilmoist) # I took the minimum value from the year because I just want to know how many years a fire occurs, not how many times it catches fire that year. This is as good an approximation as any, if you ask me.  
  soilmoist #Sum of soil moisture for one year? What is this? 
  
  # Calculate sm_fire value from soilmoist
  slmsts <- calc.slmsts(slxsand, slxclay)
  slpots <- calc.slpots(slxsand, slxclay)
  slbs   <- calc.slbs(slxsand, slxclay)
  soilcp <- calc.soilcp(slmsts, slpots, slbs)
  
  sm_fire <- (soilmoist-soilcp)/(slmsts-soilcp)
  sm_fire <- data.frame(sm_fire)
  
  # Compile into dataframe
  if(i==1){
    smfire_df <- sm_fire
  } else {
    smfire_df <- rbind(smfire_df, sm_fire)
  } # Close data frame loop
  nc_close(sand.test) 
} # Close i loop

# Find threshold for fire to occur every 20 years. 
FRI_20 <- quantile(smfire_df$sm_fire, 0.05) 
FRI_20

# Find threshold for fire to occur every 5 years. 
FRI_5 <- quantile(smfire_df$sm_fire, 0.20)
FRI_5 #This returns a value around 0.02, which is what I had set for the low fire return interval in my dry climates.

library(MASS)
# I want a table with predicted sm_fire values.
sm_fire <- c(0, 0.01, 0.015, 0.02, 0.025, 0.03)
s=0
for (s in sm_fire){
  smfire_subset <- smfire_df[which(smfire_df$sm_fire <= s),]
  FRI <- as.numeric(1/(fractions(length(smfire_subset)/length(smfire_df$sm_fire))))
  FRI <- data.frame(FRI=FRI,
                    sm_fire=s)
  if(s==0){
    FRI_df <- data.frame(FRI)
  } else {
    FRI_df <- rbind(FRI_df, FRI)
  }
}

#Generate a histogram
hist(smfire_df$sm_fire,
     col="blue",
     xlab="SM_FIRE",
     main="Dry Climate and Clay Soil")

length(smfire_df$sm_fire)
smfire_subset <- smfire_df[which(smfire_df$sm_fire <= 0.025),]
smfire_subset
FRI <- as.numeric(1/fractions(length(smfire_subset)/length(smfire_df$sm_fire)))
FRI
