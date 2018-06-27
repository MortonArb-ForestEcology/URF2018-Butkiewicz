# How Ed calculates the fire threshold if sm_fire < 0
smfire.neg <- function(slmsts, slpots, smfire, slbs){
        grav=9.80665
        # soil(nsoil)%soilfr = soil(nsoil)%slmsts * Â ( soil(nsoil)%slpots / (sm_fire * 1000. / grav)) ** ( 1. / soil(nsoil)%slbs)
        # soilfr = slmsts * ( slpots / (sm_fire * 1000. / grav)) ** ( 1. / slbs)
	soilfr <- slmsts*((slpots/(smfire * 1000/9.80665))^(1/slbs))
	
	return(soilfr)
}

# How Ed calculates the fire threshold if sm_fire > 0
smfire.pos <- function(slmsts, soilcp, smfire){
	soilfr <- soilcp + smfire * (slmsts - soilcp)
	return(soilfr)
}

# Values from ED table
slmsts = 0.407210 # Saturated water capacity
slpots = -0.114261521 # Saturated water potential
slbs = 4.629000 # Exponent
soilcp = 0.073495043 # Dry soil capacity

# Water Potential Values
smfire.neg(slmsts, slpots, smfire=-0.11, slbs) # Close to saturation
smfire.neg(slmsts, slpots, smfire=-3.1, slbs) # Close to Dry
smfire.neg(slmsts, slpots, smfire=-3.1/2, slbs) # Close to Dry
smfire.neg(slmsts, slpots, smfire=-0.001, slbs) # Gives value similar to SMFIRE=1
smfire.neg(slmsts, slpots, smfire=-0.5, slbs) # Close to SMFIRE=0.1

smfire.neg(slmsts, slpots, smfire=slpots*.2, slbs) # Close to saturation
smfire.neg(slmsts, slpots, smfire=slpots*.5, slbs) # Close to saturation
smfire.neg(slmsts, slpots, smfire=-3.1*.2, slbs) # Close to saturation
smfire.neg(slmsts, slpots, smfire=-3.1*.5, slbs) # Close to saturation


# fraction saturation (I think)
smfire.pos(slmsts, soilcp, smfire=0.1)
smfire.pos(slmsts, soilcp, smfire=1)
smfire.pos(slmsts, soilcp, smfire=0.5)
smfire.pos(slmsts, soilcp, smfire=0.2)
