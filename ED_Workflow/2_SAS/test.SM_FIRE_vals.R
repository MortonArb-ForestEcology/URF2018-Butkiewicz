# Test code for looking at SM_Fire.

# ------------------------------------------------------------------------------------------------------------------------------------ #
# The following is only a set of code for some conceptual understanding of the smfire calculations. There's no need to execute it, I'm
# just keeping it in case I want to look at it again. 
# ------------------------------------------------------------------------------------------------------------------------------------ #

#source("../0_setup/ED_Calcs_Soil_Fire.R")
#source("./SAS.ED2.R")

#file.edit("./compile_SAS_runs.R") #This file has the example loops that I need to look at. I can't make heads or tails of them. 

#slbs <- calc.slbs(slxsand = 0.38,slxclay = 0.25) 
#slbs #This gives me the B exponent--it's a relative term and appears to be unitless. 
#slmsts <- calc.slmsts(slxsand = 0.38,slxclay = 0.25)
#slmsts #This gives me the soil moisture at saturation. In this case it's 0.44179 when the soil is saturated. That means that SM_Fire will have to be well below this capacity, I believe. 
#slpots <- calc.slpots(slxsand = 0.38,slxclay = 0.25)
#slpots #This gives me the soil moisture potential at saturation. 
#soilcp <- calc.soilcp(slbs = slbs,slpots = slpots,slmsts = slmsts)
#soilcp #This gives me dry soil capacity. Al of the above values are used to calculate this. 
#smfire.pos(slmsts = slmsts,soilcp = soilcp,smfire = 0.2) #SO I think that if SM_FIRE is a direct value, then MAYBE try something like
#smfire=0.2*slmsts
#smfire
#smfire.pos(slmsts = slmsts,soilcp = soilcp,smfire = smfire)

# ------------------------------------------------------------------------------------------------------------------- #
# The following code is for actually looping through the functions and trying to understand what I'm looping through. #
# ------------------------------------------------------------------------------------------------------------------- #

source("../0_setup/ED_Calcs_Soil_Fire.R")
source("./SAS.ED2.R")

SAS.ED2(dir.analy = "../1_spin_initial/URF_2018_spininit.v1/CD-SC-FN-TL-IM/analy",dir.histo = "../1_spin_initial/URF_2018_spininit.v1/CD-SC-FN-TL-IM/histo/",