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

# Soil Moisture at saturation
calc.slmsts <- function(slxsand, slxclay){
  # Soil moisture at saturation [ m^3/m^3 ]
  (50.5 - 14.2*slxsand - 3.7*slxclay) / 100.
}

calc.slpots <- function(slxclay, slxsand){
  # Soil moisture potential at saturation [ m ]
  -1. * (10.^(2.17 - 0.63*slxclay - 1.58*slxsand)) * 0.01
  
}
calc.slbs   <- function(slxclay, slxsand){
  # B exponent (unitless)
  3.10 + 15.7*slxclay - 0.3*slxsand
}

calc.soilcp <- function(slmsts, slpots, slbs){         
  # Dry soil capacity (at -3.1MPa) [ m^3/m^3 ].
  # soil(nslcon)%soilcp  = soil(nslcon)%slmsts                                        &
  #   *  ( soil(nslcon)%slpots / (soilcp_MPa * wdns / grav))       &
  #   ** (1. / soil(nslcon)%slbs)
  soilcp_MPa = -3.1
  wdns = 1.000e3    
  grav=9.80665
  
  slmsts *  (slpots / (soilcp_MPa * wdns / grav)) ^ (1./slbs)
}
