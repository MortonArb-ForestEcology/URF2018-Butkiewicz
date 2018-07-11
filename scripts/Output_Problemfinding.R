##########################################################
# Investigating the Output files of our initial Spin-Ups # 
##########################################################

library(ncdf4)
#Run this in the 1_spin_initial folder, because then everything's right there.

path.file <- "/Users/Cori/Research/Morton_Arb/Project/Die/"
ann.files  <- dir(path.file, ".nc") #Should give us the actual output only, and ignore the nc.var files.

in.base  <- "URF2018_spininit.v1/"
out.base <- "Variable_Assessment/"
if(!dir.exists(out.base)) dir.create(out.base)

#The extracted output is in the format ED2.1801.nc
blckyr  <- 50
yrind <- which(strsplit(ann.files,".")[[1]]=="nc") #Haha here's a problem: this object (and all the ones after it) are empty. Idk why. 
yeara <- as.numeric(strsplit(ann.files,".")[[1]][yrind+1]) #first year (I think)
yearz <- as.numeric(strsplit(ann.files,".")[[length(files.output)]][yrind+1]) #last full year (I think)
yrs <- seq(yeara+1,yearz,by=blckyr) # The years we're going ot use as time steps for the demography. I don't know what this means either. 
nsteps <- length(yrs) # According to Christy, the number of blocks = the number steps we'll have, but I don't know what this means

for (y in yrs){
  now <- ncdf4::nc_open(file.path(path.file,ann.files[y-yeara+1]))
  ind <- which(yrs == y)
  
  #Grab variable to see how many cohorts there are
  ipft      <- ncdf4::ncvar_get(now,'PFT')
  
  #---------------------------------------
  # organize into .css variables (Cohorts)
  # Note: all cohorts from a time slice are assigned to a single patch representing a stand of age X
  #---------------------------------------
  css.tmp <- matrix(nrow=length(ipft),ncol=10)
  colnames(css.tmp) <- c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "Avgrg")
  
  css.tmp[,"time"  ] <- rep(yeara,length(ipft))
  css.tmp[,"patch" ] <- rep(floor((y-yeara)/blckyr)+1,length(ipft))
  css.tmp[,"cohort"] <- 1:length(ipft)
  css.tmp[,"dbh"   ] <- ncdf4::ncvar_get(now,'DBH')
  css.tmp[,"hite"  ] <- ncdf4::ncvar_get(now,'HITE')
  css.tmp[,"pft"   ] <- ipft
  css.tmp[,"n"     ] <- ncdf4::ncvar_get(now,'NPLANT')
  css.tmp[,"bdead" ] <- ncdf4::ncvar_get(now,'BDEAD')
  css.tmp[,"balive"] <- ncdf4::ncvar_get(now,'BALIVE')
  css.tmp[,"Avgrg" ] <- rep(0,length(ipft))
  
  #save big .css matrix
  if(y==yrs[1]){
    css.big <- css.tmp
  } else{
    css.big <- rbind(css.big,css.tmp)
  }
  
  pss.big[ind,"time"]  <- 1800
  pss.big[ind,"patch"] <- floor((y-yeara)/blckyr)+1
  pss.big[ind,"trk"]   <- 1
  pss.big[ind,"age"]   <- y-yeara
  # Note: the following are just place holders that will be overwritten post-SAS
  # pss.big[ind,6]  <- ncdf4::ncvar_get(now,"AREA")
  pss.big[ind,"water"]  <- 0.5 
  pss.big[ind,"fsc"]  <- ncdf4::ncvar_get(now,"FAST_SOIL_C")
  pss.big[ind,"stsc"]  <- ncdf4::ncvar_get(now,"STRUCTURAL_SOIL_C")
  pss.big[ind,"stsl"] <- ncdf4::ncvar_get(now,"STRUCTURAL_SOIL_L")
  pss.big[ind,"ssc"] <- ncdf4::ncvar_get(now,"SLOW_SOIL_C")
  pss.big[ind,"psc"] <- 0
  pss.big[ind,"msn"] <- ncdf4::ncvar_get(now,"MINERALIZED_SOIL_N")
  pss.big[ind,"fsn"] <- ncdf4::ncvar_get(now,"FAST_SOIL_N")
  
  ncdf4::nc_close(now)
}
