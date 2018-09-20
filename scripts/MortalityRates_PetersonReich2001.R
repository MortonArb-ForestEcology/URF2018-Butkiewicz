# PlayinTrying to reproduce the mortality rates reported in Peterson & Reich 2001 Ecological applications 11:914-927

# This is what Peterson & Reich did for their analysis
# MORT <- lm(log(MORT) ~ FIRE + I(FIRE^2) + DIAM + I(DIAM^2))
# We can turn this into a function by adding the coefficients reported in Table 6
mort.pin <- function(FIRE, DIAM){
	log.odds <- 13.54 + 0.47*FIRE + -0.04*(FIRE^2) + -8.62*log(DIAM) + 1.24*(log(DIAM)^2)
	mrate <- 1/(1+exp(-log.odds))
	return(mrate)
}
mort.bur <- function(FIRE){
	log.odds <- -1.54 + -3.0*FIRE 
	mrate <- 1/(1+exp(-log.odds))
	return(mrate)
}

# By plu
diam.range <- seq(5, 60, by=5) # We can generate a range of diameters to feed into our function; # Note, they only really look at 5-60 cm range
mrate <- mort.pin(FIRE=1, DIAM=diam.range)

plot(mrate ~ diam.range, type="b")


