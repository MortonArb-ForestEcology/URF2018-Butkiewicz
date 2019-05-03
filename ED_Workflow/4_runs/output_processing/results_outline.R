# Clean script to work with for the results
# All dataframes will be labeled as dat.name, where the name somewhat describes the content. 

# Load necessary packages
library(ggplot2) # Will be used for graphs in scrip
library(car) # Will be used to recode some data in the script. 
# library(dplyr) # Commented out for now because I might use it later but don't currently need it

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

############################
# DESCRIBE ABSOLUTE CHANGE #
############################

# Prepare datatable that will be used for analyzing aboveground biomass
dat.analy <- aggregate(dat.all[,c("w.agb")], by=dat.all[,c("SLXSAND","SM_FIRE","RUNID","year","fire")], FUN=sum, na.rm = T) # Sum agb across pfts for each RUNID and year
colnames(dat.analy) <- c("SLXSAND","SM_FIRE","RUNID","year","fire","agb") # Aggregate gives you a column labeled "x" for some reason, so here we change it to agb

# ---------------------------------
# Differences among starting points
# ---------------------------------

# Subset aboveground biomass for the first 25 years of each simulation. 
dat.tmp1 <- subset(dat.analy, subset = dat.analy$year<=min(dat.analy$year)+25) # Subset first 25 years of the simulation

# ANOVA test to see if they differ between runs
agb_1.aov <- aov(agb ~ RUNID, data = dat.tmp1)
summary(agb_1.aov)
agb_1.hsd <- TukeyHSD(agb_1.aov)
which(agb_1.hsd$RUNID[,4] > 0.05) 
# Only starting points that are not different: s5-f1

# Two-tailed ANOVA test
agb_1.aov2 <- aov(agb ~ SLXSAND + SM_FIRE + SLXSAND:SM_FIRE, data = dat.tmp1)
summary(agb_1.aov2)
agb_1.hsd2 <- TukeyHSD(agb_1.aov2)

  # Soils
  which(agb_1.hsd2$SLXSAND[,c("p adj")] <= 0.05) # Starting biomass significantly different in all soils
  which(agb_1.hsd2$SLXSAND[,c("p adj")] > 0.05) # No starting biomass is the same
  agb_1.hsd2$SLXSAND[,c("diff")] # Aboveground biomass always starts off larger in sils with more sand? 

  # Fire
  which(agb_1.hsd2$SM_FIRE[,c("p adj")] <= 0.05)
  which(agb_1.hsd2$SM_FIRE[,c("p adj")] > 0.05) # Starting biomass only the same between SM_FIRE = 0.0 and 0.01 (which makes sense since neighter scenario catches fire)
  agb_1.hsd2$SM_FIRE[,c("diff")] # Biomass significantly lower in scenarios with more fire. 
  
  # Interaction
  which(agb_1.hsd2$`SLXSAND:SM_FIRE`[,c("p adj")] <= 0.05) 
  which(agb_1.hsd2$`SLXSAND:SM_FIRE`[,c("p adj")] > 0.05) # Interaction not significant in some scenarios
  
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
dat.tmp2 <- subset(dat.analy, subset = dat.analy$year>=max(dat.analy$year)-25)
dat.sd <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd")
dat.tmp2 <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
dat.tmp2 <- merge(dat.tmp2, dat.sd)
colnames(dat.tmp2) <- c("SLXSAND","SM_FIRE","RUNID","agb_L","agb_L.sd")
  
# Finish dat.analy table
dat.analy <- merge(dat.tmp1, dat.tmp2)
rm(dat.tmp1, dat.tmp2, dat.sd) # Remove unnecessary variables
dat.analy$diff <- dat.analy$agb_L - dat.analy$agb_L

############################
# DESCRIBE RELATIVE CHANGE #
############################

# --------------------------------------------
# Differences among relative change in biomass
# --------------------------------------------

##############################
# IDENTIFY DRIVERS OF CHANGE #
##############################

# -----------------------------------------------
# Convert treatments to actual fires and moisture
# -----------------------------------------------

# ------------------------------------------------------------
# Statistics on effect of actual fires and moisture on biomass
# ------------------------------------------------------------