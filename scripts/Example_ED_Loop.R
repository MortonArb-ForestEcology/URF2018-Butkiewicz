# Doing some EDA to make sure the ED runs are at least somewhat on par with others
library(ncdf4)
library(car)
setwd("~/Desktop/Research/PalEON_CR/PalEON_MIP_Site/")

# ------------------------------------------------
# Setting up to compare the inital data from the models
# ------------------------------------------------
model.dir <- "phase1a_model_output"
#model.dir <- "phase1a_model_output/"

#~/Desktop/PalEON_CR/PalEON_MIP_Site/phase1a_model_output
# Models for which we have data
model.list <- dir(model.dir)
model.list

# Sites
site.list <- c("PHA", "PHO", "PUN", "PBL", "PDL", "PMB")
#site.list <- c("PHA", "PBL", "PDL", "PMB")
# useful numbers
yr2sec <- 1/(365*24*60*60)
mo2sec <- 1/(12*24*60*60)

# ------------------------------------------------------------------------
# Extracting Variables names to make life easier
# ------------------------------------------------------------------------
# Setting up directories to pull an example file
dir.ed <- file.path(model.dir, "ED2.v7", site.list[1])
files.ed <- dir(dir.ed)


# Opening an example file from each model
ed          <- nc_open(file.path(dir.ed, files.ed[1]))

# extracting variable names
ed.var <- names(ed$var)

# PFT-level variables need to be dealt with slightly differently than single-string variables
var.diversity <- c("BA", "Dens", "Fcomp", "PFT", "fpc", "pft-vegc", "pft-lai", "pft-npp", "pft-diam", "pft-height", "nind", "estrate", "AGB.pft")

# summary(clm$var)
# summary(ncvar_get(clm, "Fcomp"))
#ncvar_get(clm, "pft")

# -----------------------------------
# Soil variables have different layers and need to be indexed accordingly
# -----------------------------------
#   These indices lump things into the 0.5 and 1.5 m depths of lpj.g
soil.var <- c("SoilDepth", "SoilMoist", "SoilTemp")

soil.ed <- ncvar_get(ed, "SoilDepth")
soil.ed.5 <- which(abs(soil.ed)<=0.5); vol.ed <- vector(length=length(soil.ed))
for(i in 1:(length(soil.ed)-1)){
  vol.ed[length(soil.ed)] <- abs(soil.ed[length(soil.ed)])
  vol.ed[i] <- abs(abs(soil.ed[i]) - abs(soil.ed[i+1]))
  }


# Closing files
nc_close(ed); 
# ------------------------------------------------------------------------
# EXTRACTING MODEL OUTPUTS
# ------------------------------------------------------------------------
# -----------------------------------
# ED 2.1
# -----------------------------------
ed <- list()
ed.diversity <- list()
for(s in 1:length(site.list)){
  dir.ed <- file.path(model.dir, "ED2.v7", site.list[s])
  files.ed <- dir(dir.ed)
  
  #  nee.temp <- npp.temp <- rh.temp <- ah.temp <- gpp.temp <- vector()
  ed.var.list <- list()
  div.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  for(i in 1:length(files.ed)){
    ncMT <- nc_open(file.path(dir.ed, files.ed[i]))
    for(v in 1:length(ed.var)){
      if(i == 1) temp <- vector() else temp <- ed.var.list[[v]]
      if(ed.var[v] %in% var.diversity[1:3]){
      	temp <- c(temp, colSums(ncvar_get(ncMT, ed.var[[v]])))
      } else if(ed.var[v] %in% soil.var[2:3]){
        soil.temp <- t(ncvar_get(ncMT, ed.var[v]))[,soil.ed.5]
        for(q in 1:ncol(soil.temp)){
        	soil.temp[,q] <- soil.temp[,q]* vol.ed[soil.ed.5[q]]/sum(vol.ed[soil.ed.5])
        }
        temp <- c(temp, rowSums(soil.temp))
      } else {      
      temp <- c(temp, ncvar_get(ncMT, ed.var[v])) }
      ed.var.list[[v]] <- temp
    }
	# ----------------------
	# Adding in Fraction Evergreen Tree, Deciduous Tree, Grass
	# ----------------------
    if(i == 1){ evg <- decid <- grass <- vector() 
    } else { 
	  evg   <- ed.var.list[["Evergreen"]]
	  decid <- ed.var.list[["Deciduous"]]
	  grass <- ed.var.list[["Grass"]]
    }

    ed.var.list[["Evergreen"]] <- c(evg  , colSums(ncvar_get(ncMT, "Fcomp")[6:8 ,]))
    ed.var.list[["Deciduous"]] <- c(decid, colSums(ncvar_get(ncMT, "Fcomp")[9:11,]))
    ed.var.list[["Grass"    ]] <- c(grass, colSums(ncvar_get(ncMT, "Fcomp")[c(1,5,12:16),]))
    # ----------------------
    nc_close(ncMT)      
  }
  names(ed.var.list) <- c(ed.var, "Evergreen", "Deciduous", "Grass")
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:(length(ed.var)+3)){
    if(s == 1){
      ed[[v]] <- data.frame(ed.var.list[[v]]) 
    } else {
      ed[[v]][,s] <- ed.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(ed) <- c(ed.var, "Evergreen", "Deciduous", "Grass")
for(i in 1:length(ed)){
  names(ed[[i]]) <- site.list
}
# -----------------------------------


# ------------------------------------------------------------------------
# ORGANIZING MODEL OUTPUTS BY VARIABLE
# ------------------------------------------------------------------------
names(ed) 
