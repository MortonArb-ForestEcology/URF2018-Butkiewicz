# ------------------------------------------------------------------------------------
# This file compiles the steady-state approximation for an accelerated model spinup
# at individual points (this will need to be modified to work efficiently with spatially 
# files)
#
# References: 
#   1. Xia, J.Y., Y.Q. Luo, Y.-P. Wang, E.S. Weng, and O. Hararuk. 2012. A semi-analytical 
#      solution to accelerate spin-up of a coupled carbon and nitrogen land model to 
#      steady state. Geoscientific Model Development 5:1259-1271.
#
#   2. Xia, J., Y. Luo, Y.-P. Wang, and O. Hararuk. 2013. Traceable components of terrestrial 
#      carbon storage capacity in biogeochemical models.  Global Change Biology 19:2104-2116
#
#
# Original ED SAS solution Script at PalEON modeling HIPS sites:
# Jaclyn Hatala Matthes, 2/18/14
# jaclyn.hatala.matthes@gmail.com
#
# Modifications for greater site flexibility & updated ED
# Christine Rollinson, Aug 2015
# crollinson@gmail.com
#
# Adaptation for regional-scale runs (single-cells run independently, but executed in batches)
# Christine Rollinson, Jan 2016
# crollinson@gmail.com
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# NOTES ON THE SAS SPINUP:
# ------------------------------------------------------------------------------------
# The SAS (semi-analytical solution) should be perfomed on ED runs 
#          *******WITH DISTURBANCE TURNED OFF*******
# Turning off the disturbance (both treefall & fire) means the model will run with a 
# single patch AND we have a robust patch saying what a theoretical old growth looks like
#
# FSC = Fast soil C 
# SSC = Structural soil C
# SSL = structural soil L
# MSN = Mineralized Soil N
# FSN = Fast soil N
# ------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------
# Setting things up to run equations, etc
# ------------------------------------------------------------------------------------
#---------------------------------------
# Define File Structures & steps
# Additional fixed constants and file paths that don't depend on the site
#---------------------------------------
# Site Info
#Setup analysis file structure
# in.base  <- "/home/crollinson/URF2018-Butkiewicz/ED_Workflow/1_spin_initial/URF2018_spininit.v1/"
# out.base <- "/home/crollinson/URF2018-Butkiewicz/ED_Workflow/2_SAS/SAS_init_files.v1/"

in.base  <- "../1_spin_initial/URF2018_spininit.v3/"
out.base <- "SAS_init_files.v3/"

if(!dir.exists(out.base)) dir.create(out.base)

# Load site characteristic table
expdesign <- read.csv("../0_setup/ExperimentalDesign.csv")
summary(expdesign)

blckyr  <- 50 #number of years to chunk data by
disturb <- 0.005 # the treefall disturbance rate you will prescribe in the actual runs (or close to it)
yrs.met <- 30 # The number of met years in the spinup loop

kh_active_depth = -0.2

# pft   <- c(5,6,8,9,10,11) #set of PFTs used in analysis
# dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) # days per month
sufx  <- "g01.h5"

expdesign <- expdesign[expdesign$RunID %in% dir(in.base),] # Do what we've spunup already
expdesign <- expdesign[!expdesign$RunID %in% dir(out.base),] # Don't do anything we've already done the SAS for
#---------------------------------------

# ------------------------------------------------------------------------------------
# Running the SAS Solution
# ------------------------------------------------------------------------------------
source("../0_setup/ED_Calcs_Soil_Fire.R")
source("SAS.ED2.R")
for(s in 1:nrow(expdesign)){
  prefix <- expdesign$RunID[s]
  
  cat("***** Processing site:", paste(prefix), "\n")
  # Read run settings % Sand & % CLAY from table
  slxsand <- expdesign$SLXSAND[s]
  slxclay <- expdesign$SLXCLAY[s]
  lat <- round(expdesign$latitude[s],2)
  lon <- round(expdesign$latitude[s],2)
  dir.analy <- file.path(in.base, prefix, "analy")
  dir.histo <- file.path(in.base, prefix, "histo")
  outdir <- file.path(out.base, prefix)
  SAS.ED2(dir.analy=dir.analy, dir.histo=dir.histo, outdir=outdir, 
          prefix, lat, lon, 
          block=50, yrs.met=30,
          treefall=0.005, sm_fire=0, fire_intensity=0, slxsand=slxsand, slxclay=slxclay,
          decomp_scheme=2
          ) 
} # End Site Loop!
# -------------------------------------
