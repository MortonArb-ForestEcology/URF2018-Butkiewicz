# Clean script to work with for the results
# All dataframes will be labeled as dat.name, where the name somewhat describes the content. 

# Load necessary packages
library(nlme)
library(car) # Will be used to recode some data in the script. 
# library(dplyr) # Commented out for now because I might use it later but don't currently need it
library(ggplot2) # Will be used for graphs in scrip

# Load all data and format datatable. Note that filepaths will change between computers. 

# path.google <- "/Volumes/GoogleDrive/My Drive/URF 2018 Butkiewicz" # Filepath for Christy when she wants to run the script herself. 
path.google <- "/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/"
# dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")
dat.all <- read.csv(file.path(path.google, "v5_tables", "output_runs.v5.csv")) # Filepath for Christy when she wants to run the script herself. 
summary(dat.all) 
# When reading the datatable: 
  # RUNID = the identity of the treatment. Coded so that the first half (s#) corresponds to a soil texture and the second half (f#) corresponds to a fire setting. 
  # year = from 0 to 214. 0 corresponds to the year 1800 CE and 214 to 2014 CE. 
  # w.agb = Aboveground Biomass (kg C m-2)
  # w.dens = Density (plants m-2)
  # w.dbh = Diameter at Breast Height (cm)
  # w.ba = Basal Area (cm2 m-2)
  # w.dens.tree = Density of Trees with DBH > 10 cm (trees m-2)
  # w.dbh.tree = Diameter at Breast Height of Trees with DBH > 10 cm (cm2 m-2)
  # w.ba.tree = Basal Area of Trees with DBH > 10 cm (cm2 m-2)
  # soil_moist = Soil Moisture, proportion of saturation
  # fire = Binary variable describing whether or not fire occured in that year of the simulation. 0 = no fire occurred, 1 = fire occured. 

# Add columns that individually specify treatment values
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("SLXSAND","SM_FIRE") # Name the columns after their variables in ED2. SLXSAND refers to the sand fraction, and SM_FIRE refers to the fire setting.  
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
rm(factors) # Remove unnecessary variables

# Recode dataframe so that: 1) RUNID codes are shown as the parameter values, and 2) Specifying the order that those factors should be listed in. 
dat.all$SLXSAND <- car::recode(dat.all$SLXSAND, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'") # Recode soil ID's as soil texture values
dat.all$SM_FIRE <- car::recode(dat.all$SM_FIRE, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'") # Recode fire ID's as fire threshold (SM_FIRE) values
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0, 0.01, 0.02, 0.03, 0.04)) #stores SM_FIRE as a factor and telling it what order we should always list things in

# --------
# Figure 1
# ---------

############################
# DESCRIBE ABSOLUTE CHANGE #
############################

# Prepare datatable that will be used for analyzing aboveground biomass
dat.analy <- aggregate(dat.all[,c("w.agb")], by=dat.all[,c("SLXSAND","SM_FIRE","RUNID","year","fire")], FUN=sum, na.rm = T) # Sum agb across pfts for each RUNID and year
colnames(dat.analy) <- c("SLXSAND","SM_FIRE","RUNID","year","fire","agb") # Aggregate gives you a column labeled "x" for some reason, so here we change it to agb
dat.fire <- dat.analy[,c("SLXSAND","SM_FIRE","RUNID","year","fire")]

# ---------------------------------
# Differences among starting points
# ---------------------------------

# # Subset aboveground biomass for the first 25 years of each simulation. 
dat.tmp1 <- subset(dat.analy, subset = dat.analy$year<min(dat.analy$year)+25) # Subset first 25 years of the simulation
length(dat.tmp1$year)/25 # Check to make sure that the subset worked
# 
# # ANOVA test to see if they differ between runs
agb_1.aov <- aov(agb ~ RUNID, data = dat.tmp1)
summary(agb_1.aov)
# agb_1.hsd <- TukeyHSD(agb_1.aov)
# which(agb_1.hsd$RUNID[,4] > 0.05) 
# # Only starting points that are not different: s5-f1

agb_soil.aov <- aov(agb ~ SLXSAND, data = dat.tmp1)
summary(agb_soil.aov)
TukeyHSD(agb_soil.aov)

agb_fire.aov <- aov(agb ~ SM_FIRE, data = dat.tmp1)
summary(agb_fire.aov)
TukeyHSD(agb_fire.aov)

# test <- lme(agb ~ SLXSAND*SM_FIRE, random=list(year=~1), data = dat.analy)
# summary(test)
# 
# # Two-tailed ANOVA test
# agb_1.aov2 <- aov(agb ~ SLXSAND + SM_FIRE + SLXSAND:SM_FIRE, data = dat.tmp1)
# summary(agb_1.aov2)
# agb_1.hsd2 <- TukeyHSD(agb_1.aov2)
# 
#   # Soils
#   which(agb_1.hsd2$SLXSAND[,c("p adj")] <= 0.05) # Starting biomass significantly different in all soils
#   which(agb_1.hsd2$SLXSAND[,c("p adj")] > 0.05) # No starting biomass is the same
#   agb_1.hsd2$SLXSAND[,c("diff")] # Aboveground biomass always starts off larger in sils with more sand? 
# 
#   # Fire
#   which(agb_1.hsd2$SM_FIRE[,c("p adj")] <= 0.05)
#   which(agb_1.hsd2$SM_FIRE[,c("p adj")] > 0.05) # Starting biomass only the same between SM_FIRE = 0.0 and 0.01 (which makes sense since neighter scenario catches fire)
#   agb_1.hsd2$SM_FIRE[,c("diff")] # Biomass significantly lower in scenarios with more fire. 
#   
#   # Interaction
#   which(agb_1.hsd2$`SLXSAND:SM_FIRE`[,c("p adj")] <= 0.05) 
#   which(agb_1.hsd2$`SLXSAND:SM_FIRE`[,c("p adj")] > 0.05) # Interaction not significant in some scenarios
  
# ----------------------------------------------
# Differences among absolute changes in biomass
# ----------------------------------------------

# Find mean aboveground biomass for the first 25 years of each simulation
dat.sd <- aggregate(dat.tmp1["agb"], by = dat.tmp1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd") # agb_1 refers to mean aboveground biomass for the first 25 years of the simulation. 
dat.tmp1 <- aggregate(dat.tmp1["agb"], by = dat.tmp1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
dat.tmp1 <- merge(dat.tmp1, dat.sd)
colnames(dat.tmp1) <- c("SLXSAND","SM_FIRE","RUNID","agb_1","agb_1.sd")
  
# Find mean aboveground biomass for the last 25 years of each simulation
dat.tmp2 <- subset(dat.analy, subset = dat.analy$year>max(dat.analy$year)-25)
# dat.sd <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd")
# dat.tmp2 <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
# dat.tmp2 <- merge(dat.tmp2, dat.sd)
colnames(dat.tmp2) <- c("SLXSAND","SM_FIRE","RUNID","agb_L","agb_L.sd")

dat.test <- merge(dat.tmp1, dat.tmp2)
dat.test$diff <- dat.test$agb_L - dat.test$agb_1
dat.test$pdiff <- dat.test$diff / dat.test$agb_1

test2 <- lme(pdiff ~ SLXSAND*SM_FIRE, random = list(year = ~1), data = dat.test)
summary(test2)

test <- lme(agb ~ SLXSAND*SM_FIRE, random=list(year=~1), data = dat.tmp1)
summary(test)
  
# Finish dat.analy table
dat.analy <- merge(dat.tmp1, dat.tmp2)
  rm(dat.tmp1, dat.tmp2, dat.sd) # Remove unnecessary variables
dat.analy$diff <- dat.analy$agb_L - dat.analy$agb_1
dat.analy$p.diff <- dat.analy$diff / dat.analy$agb_1 # p.diff stands for proportional difference

# Stats tests for soils
soil.aov <- aov(diff ~ SLXSAND, data = dat.analy)
summary(soil.aov)

soil.lm  <- lm(diff ~ SLXSAND, data=dat.analy) # Effects parameterizaiton --> relative effects
summary(soil.lm) # None different from each other
anova(soil.lm)

soil.lm2  <- lm(diff ~ SLXSAND-1, data=dat.analy) # looks at the effect of each category relative to 0 (force overall itnercept through 0); means parameterization --> absolute effects
summary(soil.lm2) # All different from 0

# Stats tests for fire
fire.aov <- aov(diff ~ SM_FIRE, data = dat.analy)
summary(fire.aov)
TukeyHSD(fire.aov)

fire.lm <- lm(diff ~ SM_FIRE, data = dat.analy)
summary(fire.lm)

fire.lm2 <- lm(diff ~ SM_FIRE-1, data = dat.analy) # Looks at the effect of each category relative to 0
summary(fire.lm2)

############################
# DESCRIBE RELATIVE CHANGE #
############################

# --------------------------------------------
# Differences among relative change in biomass
# --------------------------------------------

# Stats tests for soils
psoil.aov <- aov(p.diff ~ SLXSAND, data = dat.analy)
summary(psoil.aov)

psoil.lm  <- lm(p.diff ~ SLXSAND, data=dat.analy) # Effects parameterizaiton --> relative effects
summary(psoil.lm) # None different from each other
anova(psoil.lm)

psoil.lm2  <- lm(p.diff ~ SLXSAND-1, data=dat.analy) # looks at the effect of each category relative to 0 (force overall itnercept through 0); means parameterization --> absolute effects
summary(soil.lm2) # All different from 0

# Stats tests for fire
pfire.aov <- aov(p.diff ~ SM_FIRE, data = dat.analy)
summary(pfire.aov)
TukeyHSD(pfire.aov)

pfire.lm <- lm(p.diff ~ SM_FIRE, data = dat.analy)
summary(pfire.lm)

pfire.lm2 <- lm(p.diff ~ SM_FIRE-1, data = dat.analy) # Looks at the effect of each category relative to 0
summary(pfire.lm2)

# ------------
# Soils Graph
# ------------

# -----------
# Fire Graph
# -----------

##############################
# IDENTIFY DRIVERS OF CHANGE #
##############################

# -----------------------------------------------
# Convert treatments to actual fires and moisture
# -----------------------------------------------

# Number of fires
dat.regime <- aggregate(dat.fire["fire"], by = dat.fire[c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
dat.regime <- dat.regime[,c("RUNID","SM_FIRE","SLXSAND","fire")] # Remove unnecessary pft column
colnames(dat.regime) <- c("RUNID","SM_FIRE","SLXSAND","nfire") # Rename columns. Here nfire means "number of fires"

dat.regime1 <- subset(dat.fire, subset = dat.fire$year<min(dat.fire$year)+100)
  length(unique(dat.regime1$year))
dat.regime1 <- aggregate(dat.regime1["fire"], by = dat.regime1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
  colnames(dat.regime1) <- c("SLXSAND","SM_FIRE","RUNID","fire.1")

dat.regime2 <- subset(dat.fire, subset = dat.fire$year>max(dat.fire$year)-100)
  length(unique(dat.regime2$year))
dat.regime2 <- aggregate(dat.regime2["fire"], by = dat.regime2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
  colnames(dat.regime2) <- c("SLXSAND","SM_FIRE","RUNID","fire.L")
dat.regime1 <- merge(dat.regime1, dat.regime2)
  rm(dat.regime2)
dat.regime1$diff <- dat.regime1$fire.L - dat.regime1$fire.1
dat.regime1$p.diff <- dat.regime1$diff / dat.regime1$fire.1
dat.regime1$p.diff <- recode(dat.regime1$p.diff, "'NaN'='0'")
dat.regime <- merge(dat.regime, dat.regime1)
  print(colnames(dat.regime))
  colnames(dat.regime) <- c("RUNID","SM_FIRE","SLXSAND","nfire","nfire.1","nfire.L","nfire.diff","nfire.pdiff")
# dat.analy <- merge(dat.regime, dat.analy)
  dat.test <- merge(dat.regime, dat.test)
  
test3 <- lme(pdiff ~ nfire.diff, random = list(year = ~1), data = dat.test)
summary(test3)

test <- lme(agb ~ SLXSAND*SM_FIRE, random=list(year=~1), data = dat.tmp1)
summary(test)

# Soil Moisture
dat.soil <- aggregate(dat.all[c("soil_moist")], by = dat.all[,c("SLXSAND","SM_FIRE","RUNID","year")], FUN = mean) # Shouldn't really change the values since they should be the same no matter pft

dat.soil1 <- subset(dat.soil, subset = dat.soil$year<min(dat.soil$year)+25)
  length(dat.soil1$year)/25
dat.soil1 <- aggregate(dat.soil1["soil_moist"], by = dat.soil1[c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
  colnames(dat.soil1) <- c("SLXSAND","SM_FIRE","RUNID","sm.1")
dat.soilL <- subset(dat.soil, subset = dat.soil$year>max(dat.soil$year)-25)
  length(dat.soilL$year)/25
dat.soilL <- aggregate(dat.soilL["soil_moist"], by = dat.soilL[c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
  colnames(dat.soilL) <- c("SLXSAND","SM_FIRE","RUNID","sm.L")
  
dat.soil <- merge(dat.soil1, dat.soilL)
dat.soil$sm.diff <- dat.soil$sm.L - dat.soil$sm.1
dat.soil$sm.pdiff <- dat.soil$sm.diff / dat.soil$sm.1

dat.analy <- merge(dat.soil, dat.analy)

# ------------------------------------------------------------
# Statistics on effect of actual fires and moisture on biomass
# ------------------------------------------------------------

soil.lm  <- lm(diff ~ sm.diff, data=dat.analy)
summary(soil.lm) # This doesn't make any sense. 
