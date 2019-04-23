#################################
# OUTPUT ANALYSIS, Forward Runs #
#################################

# The first part of this analysis is meant to find patterns in the data. 
# ------------------------------------------------------------------------

# Load the necessary data. The filepaths wil have to change between computers. 
# path.google <- "/Volumes/GoogleDrive/My Drive/URF 2018 Butkiewicz"
dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")
# dat.all <- read.csv(file.path(path.google, "v5_tables", "output_runs.v5.csv"))
summary(dat.all)

# ###########################
# Prepare the full datatable
# ###########################

# Add columns that individually specify soil and fire values 
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("SLXSAND","SM_FIRE") # Name the columns after their variables in ED2. SLXSAND refers to the sand fraction, and SM_FIRE refers to the fire setting.  
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 

# Recode dataframe so that: 1) RUNID codes are shown as the parameter values, and 2) Specifying the order that those factors should be listed in. 
dat.all$SLXSAND <- car::recode(dat.all$SLXSAND, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'") 
dat.all$SM_FIRE <- car::recode(dat.all$SM_FIRE, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0, 0.01, 0.02, 0.03, 0.04)) #says SM_FIRE is a factor and telling it what order we should always list things in 

#########################################
# Preparing main figure for publication # 
#########################################

# Prepare subset dat.all to a manageable table that examines agb
# --------------------------------------------------------------

  dat.agb <- aggregate(dat.all[,c("w.agb")], by=dat.all[,c("SLXSAND","SM_FIRE","RUNID","year","fire")], FUN=sum, na.rm = T) # Sum agb across pfts for each RUNID and year
  colnames(dat.agb) <- c("SLXSAND","SM_FIRE","RUNID","year","fire","agb") # Aggregate gives you a column labeled "x" for some reason, so here we change it to agb

# Find mean agb for first and last 25 years
# ------------------------------------------

  # Calculate the mean agb for the first 25 years of the simulation
  dat.first <- subset(dat.agb, subset = dat.agb$year<=min(dat.agb$year)+25) # Create dataframe with only first 25 years
  dat.sd <- aggregate(dat.first["agb"], by = dat.first[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
  colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd") # Eases merge in line 49
  dat.first <- aggregate(dat.first["agb"], by = dat.first[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
  dat.first <- merge(dat.first, dat.sd)
  colnames(dat.first) <- c("SLXSAND","SM_FIRE","RUNID","first_agb","first_agb.sd") # Rename columns to make merge in line 61 easier

  # Calculaate the mean agb for the last 25 years of the simulation
  dat.last <- subset(dat.agb, subset = dat.agb$year>=max(dat.agb$year)-25)
  dat.sd <- aggregate(dat.last["agb"], by = dat.last[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd) # Find mean for each soil texture
  colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd")
  dat.last <- aggregate(dat.last["agb"], by = dat.last[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
  dat.last <- merge(dat.last, dat.sd)
  colnames(dat.last) <- c("SLXSAND","SM_FIRE","RUNID","last_agb", "last_agb.sd") # Rename columns to make merge in line 61 easier

# Make dataframe to compare first and last means
dat.analy <- merge(dat.first,dat.last)
rm(dat.first, dat.last, dat.sd) # Remove unnecessary variables from the environment
dat.analy$difference <- dat.analy$last_agb - dat.analy$first_agb # Creates column with difference between first and last agb through time. 

# Preliminary look at some general patterns
subset(dat.analy, subset=dat.analy$difference==max(dat.analy$difference)) # Least stability?
subset(dat.analy, subset=dat.analy$difference==min(dat.analy$difference)) # Greatest stability?

# s3-f2 (66% sand, 0.03 fire threshold) --> most unstable
# s1-f1 (93% sand, 0.04 fire threshold) --> most stable


# MAIN FIGURE FOR PAPER
# -------------------------------------

# I elected to use a bar graph because I believe it shows the simple increase most effectively. 
# Although I could demonstrate similar results using a box-and-whisker plot, the difference in agb
# is large enough that it is difficult to tell which boxes are paired, and it is difficult to tell 
# how much the agb increased between scenarios. 

library(ggplot2)
# pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/all_runs_increase.pdf")
# png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/poster_agb.pdf", width = 2000, height = 1500)
ggplot(dat.analy, aes(x = SM_FIRE, y = difference, fill=SLXSAND)) + 
  geom_bar(stat="identity", position="dodge") +
  theme_bw() + 
  # Lines 88-94 are only used for poster text. 
  # theme(axis.text.x = element_text(size = 55, margin = margin(t=20)),
  #       axis.text.y = element_text(size = 55, margin = margin(r=20)),
  #       axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
  #       axis.title.y = element_text(size = 60, face = "bold", margin = margin(t = 50, r = 20)),
  #       legend.title = element_text(size=60),
  #       legend.text = element_text(size=55, margin = margin(t = 20)),
  #       legend.key.size = unit(3, "line")) + 
  scale_fill_manual(name = "Sand\nFraction", values = c("olivedrab4","olivedrab3","lightgoldenrod3","gold3","orange")) +
  ggtitle("Change in Mean Aboveground Biomass\nbetween First and Last 25 Years")+ 
  xlab("Fire Threshold") + 
  ylab (expression(bold(paste("Change in Aboveground Biomass (Kg C", " m"^"-2",")"))))
# dev.off()

###################################
# TEST FOR  PATTERNS IN SOIL DATA # 
################################### 

# Prepare dataframe to compare across soils, averaging AGB across fire settings. 
dat.soil <- aggregate(dat.analy["difference"], by=dat.analy["SLXSAND"], FUN=mean) # Find average agb for each soil texture 
dat.sd <- aggregate(dat.analy["difference"], by=dat.analy["SLXSAND"], FUN=sd)
colnames(dat.sd) <- c("SLXSAND","sd") # Here sd stands for "standard deviation"
dat.soil <- merge(dat.soil, dat.sd) # Include standard deviation
rm(dat.sd) # Remove unnecessary variables
  
# Preliminary patterns in data
subset(dat.soil, subset=dat.soil$difference==max(dat.soil$difference)) # Least stability?
subset(dat.soil, subset=dat.soil$difference==min(dat.soil$difference)) # Greatest stability?
  
# 66% sand --> most unstable
# 93% sand --> most stable
  
# Run an ANOVA test on the patterns
# ----------------------------------

soil.aov <- aov(difference ~ SLXSAND, data=dat.analy) 
summary(soil.aov)

# Apply a Linear Model to the patterns
# -------------------------------------

soil.lm  <- lm(difference ~ SLXSAND, data=dat.analy) # Effects parameterizaiton --> relative effects
soil.lm2  <- lm(difference ~ SLXSAND-1, data=dat.analy) # looks at the effect of each category relative to 0 (force overall itnercept through 0); means parameterization --> absolute effects
summary(soil.lm)
anova(soil.lm)
summary(soil.lm2)

  
# Figure to demonstrate patterns in soil
# --------------------------------------
  
# pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/soil_increase.pdf")
ggplot(dat.soil, aes(x = SLXSAND, y = difference)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = difference - sd, ymax = difference + sd, width=0.1)) + 
  theme_bw() +  
  ggtitle("Change in Aboveground Biomass\nbetween First and Last 25 Years") + 
  xlab("Sand Fraction") + 
  ylab (expression(bold(paste("Change in Aboveground Biomass (Kg C", " m"^"-2",")"))))
dev.off()

rm(dat.soil) # Remove unnecessary variables
  
##################################
# TEST FOR PATTERNS IN FIRE DATA #
##################################
  
# Prepare dataframe to compare across fire settings, averaging agb across all soils for each setting. 
dat.fire <- aggregate(dat.analy["difference"], by=dat.analy["SM_FIRE"], FUN=mean)
dat.sd <- aggregate(dat.analy["difference"], by=dat.analy["SM_FIRE"], FUN=sd)
colnames(dat.sd) <- c("SM_FIRE","sd") # Here sd stands for "standard deviation"
dat.fire <- merge(dat.fire, dat.sd) # Include standard deviation
rm(dat.sd) # Remove unnecessary variables
  
# Preliminary test for patterns in the data
subset(dat.fire, subset=dat.fire$difference==max(dat.fire$difference)) # Least stability?
subset(dat.fire, subset=dat.fire$difference==min(dat.fire$difference)) # Greatest stability?
  
# Moderately able to catch fire (SM_FIRE = 0.02) --> least stable
# Easily able to catch fire (SM_FIRE = 0.04) --> most stable
  
# Run an ANOVA test on the patterns
# ----------------------------------
fire.aov <- aov(difference ~ SM_FIRE, data=dat.analy)
summary(fire.aov)

# Now run Tukey's Means Comparison Test
dat.analy$SM_FIRE <- as.character(dat.analy$SM_FIRE)
TukeyHSD(x=fire.aov, "SM_FIRE")
  
# Apply a Linear Model on the patterns
# -------------------------------------

# Do a lm with fire off as our reference (= control!)
fri.lm <- lm(difference ~ relevel(SM_FIRE, ref="0"), data=dat.analy)
fire.sum <- summary(fire.lm)
anova(fire.lm)
  
# How much "easy fire" decreases biomass
fire.sum$coefficients[2,1]/fire.sum$coefficients[1,1] # comparing relative change of easy fire from no fire
 

# Figure to demonstrate patterns in fire
# --------------------------------------
  
# pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/fire_increase.pdf")
ggplot2::ggplot(dat.fire, aes(x = SM_FIRE, y = difference)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = difference - sd, ymax = difference + sd, width=0.1)) + 
  theme_bw() +  
  ggtitle("Change in Aboveground Biomass\nbetween First and Last 25 Years") + 
  xlab("Fire Threshold") + 
  ylab (expression(bold(paste("Change in Aboveground Biomass (Kg C", " m"^"-2",")"))))
# dev.off()
  
#############################################
# Why does easy fire have the LEAST change? #
#############################################

# HYPOTHESIS 1: Frequent fire prevents aboveground biomass accumulation
# PREDICTION: Easy fire scenarios have more fire occurrences and more frequent fires than hard fire
# scenarios. 
# TEST: Compare fire frequencies across fire settings
    
# Prepare datatable with fire regimes for each scenario
dat.regime <- aggregate(dat.agb[c("fire")], by=dat.agb[c("RUNID","SM_FIRE","SLXSAND")], FUN=sum) # Find fire regime
dat.regime <- dat.regime[,c("RUNID","SM_FIRE","SLXSAND","fire")] # Remove unnecessary pft column
colnames(dat.regime) <- c("RUNID","SM_FIRE","SLXSAND","nfire") # Rename columns. Here nfire means "number of fires"
dat.regime$FRI <- max(dat.agb$year)/dat.regime$nfire # Calculates the fire return interval by divinding the number of fires per scenario by the number of fires
dat.regime$FRI <-car::recode(dat.regime$FRI, "'Inf'='214'")

# Run an ANOVA test on FRI
# -------------------------
fri.aov <- aov(FRI ~ SM_FIRE, data=dat.regime)
summary(fri.aov)

# Tukey's Means Comparison Test
TukeyHSD(x=fri.aov, "SM_FIRE")

# Figures comparing fire regimes across fire thresholds
# -------------------------------------------------------

# Prepare tables to graphically compare means 
dat.fri <- aggregate(dat.regime[c("nfire","FRI")], by = dat.regime[c("SM_FIRE")], FUN = mean)
dat.sd <- aggregate(dat.regime[c("nfire","FRI")], by = dat.regime[c("SM_FIRE")], FUN = sd)
colnames(dat.sd) <- c("SM_FIRE","nfire.sd","FRI.sd")
dat.fri <- merge(dat.fri, dat.sd)

  # ---------------------------------
  # Figure comparing number of fires
  # ---------------------------------

  library(ggplot2)
  # pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/nfire_within_SMFIRE.pdf")
  ggplot(dat.fri, aes(x = SM_FIRE, y = nfire)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = nfire - nfire.sd, ymax = nfire + nfire.sd, width=0.1)) + 
    theme_bw() +  
    xlab("Fire Threshold") + 
    ylab (expression(bold(paste("Number of Fires")))) + 
    ggtitle("Number of Fires across Soil Textures")
  # dev.off()
  # ---------

  # Figure to comparing fire return intervals
  # -----------------------------------------
  
  # pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/FRI_within_SMFIRE.pdf")
  ggplot(dat.fri, aes(x = SM_FIRE, y = FRI)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = FRI - FRI.sd, ymax = FRI + FRI.sd, width=0.1)) + 
    theme_bw() +  
    xlab("Fire Threshold") + 
    ylab (expression(bold(paste("Fire Return Interval (years)")))) + 
    ggtitle("Fire Return Intervals across Soil Textures")
  # dev.off()
  # ---------
  
# HYPOTHESIS 2: Fire return intervals changed the least between the spinup and the final runs in the 
# easy fire setting. 
# PREDICTION: Easy fire scenarios have changed 
# TEST 1: Test to see which scenarios have a significant change in FRI between spinup and runs. 
# TEST 2: Look for differences in changes in the FRI between each run. 
  
# Load FRI data from spinup. Filepaths will change between users. 
dat.spinup <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_FRI.spinfinish.v5.csv")

# Add columns that individually specify soil and fire values
RUNID <- as.character(dat.spinup$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("SLXSAND","SM_FIRE") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.spinup <- cbind(factors,dat.spinup) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.spinup$SLXSAND <- car::recode(dat.spinup$SLXSAND, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.spinup$SM_FIRE <- car::recode(dat.spinup$SM_FIRE, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.spinup$SM_FIRE <- factor(dat.spinup$SM_FIRE, levels=c(0, 0.01, 0.02, 0.03, 0.04))
dat.spinup$FRI <- car::recode(dat.spinup$FRI, "'Inf'='100'")

###########################################################################
# ANOVA TO TEST WHICH FIRE RETURN INTERVALS ARE DIFFERENT FROM EACH OTHER #
###########################################################################
  
# Find 
dat.sd <- aggregate(dat.spinup[c("FRI")], by=dat.spinup[c("SM_FIRE")], FUN=sd)
colnames(dat.sd) <- c("SM_FIRE","FRI.sd")
dat.spinup <- aggregate(dat.spinup[c("FRI")], by=dat.spinup[c("SM_FIRE")], FUN=mean)
dat.spinup <- merge(dat.spinup, dat.sd)
rm(dat.sd)
dat.spinup <- dat.spinup[,c("SM_FIRE","FRI","FRI.sd")]
dat.spinup$Phase <- "spinup"
  
  dat.compare <- dat.spinup[c("SM_FIRE","FRI")]
  colnames(dat.compare) <- c("SM_FIRE","FRI.spinup")
  
  # Have fire regimes for this table from dat.regime
  dat.temp <- subset(dat.agb, subset=dat.agb$year>=max(dat.agb$year)-100)
  lapse <- max(dat.temp$year)-min(dat.temp$year)
  lapse
  dat.temp <- aggregate(dat.all[c("fire")], by=dat.all[c("RUNID","SLXSAND","SM_FIRE")], FUN=sum) # Find fire regime
  dat.temp$FRI <- lapse/dat.temp$fire # Calculates the fire return interval by divinding the number of fires per scenario by the number of fires
  dat.temp$FRI <- car::recode(dat.temp$FRI, "'Inf'='100'")
  dat.sd <- aggregate(dat.temp[c("FRI")], by=dat.temp[c("SM_FIRE")], FUN=sd)
  colnames(dat.sd) <- c("SM_FIRE","FRI.sd")
  dat.temp <- aggregate(dat.temp[c("FRI")], by=dat.temp[c("SM_FIRE")], FUN=mean)
  dat.temp <- merge(dat.temp, dat.sd)
  dat.temp$Phase <- "run"
  dat.spinup <- rbind(dat.spinup, dat.temp)
  
  dat.compare$FRI.run <- dat.temp$FRI
  rm(dat.temp)
  dat.compare$FRI.diff <- dat.compare$FRI.spinup - dat.compare$FRI.run

  
##########################################################################
# Why does intermediate fire (not difficult fire) have the LEAST change? #
##########################################################################
  
# Hypothesis 1: Intermediate disturbance hypothesis
# Prediction: Intermediate scenarios have the most biomass out of all the scenarios; they have some 
# fire, but less than easy fire scenario
# Test: Compare mean biomass from first & last 25 year; comparing WITHIN time slices but ACROSS soils
  
  dat.idh <- aggregate(dat.analy[,c("first_agb","last_agb")], by = dat.analy[c("SM_FIRE")], FUN = mean)
  dat.sd <- aggregate(dat.analy[,c("first_agb","last_agb")], by = dat.analy[c("SM_FIRE")], FUN = sd)
  colnames(dat.sd) <- c("SM_FIRE","first_agb.sd","last_agb.sd")
  dat.idh <- merge(dat.idh, dat.sd)
  rm(dat.sd)
  
  # -----------------------------------------
  # Figure to compare first twenty-five years
  # -----------------------------------------
  
  library(ggplot2)
  pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/first_25_years")
  ggplot(dat.idh, aes(x = SM_FIRE, y = first_agb)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = first_agb - first_agb.sd, ymax = first_agb + first_agb.sd, width=0.1)) +
    theme_bw() +  
    xlab("Fire Threshold") + 
    ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")")))) + 
    ggtitle("Intermediate Disturbance Hypothesis:\nFirst 25 Years of Simulation")
  dev.off()
  
  # ----------------------------------------
  # Figure to compare last twenty-five years
  # ----------------------------------------
  
  pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/last_25_years")
  ggplot(dat.idh, aes(x = SM_FIRE, y = last_agb)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = last_agb - last_agb.sd, ymax = last_agb + last_agb.sd, width=0.1)) +
    theme_bw() +  
    xlab("Fire Threshold") + 
    ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")")))) + 
    ggtitle("Intermediate Disturbance Hypothesis:\nLast 25 Years of Simulation")
  dev.off()
  
# Hypothesis 2: Intermediate fire setting shows the greatest change in fire frequency from spinup to 
# 2015
# ----------
  
# Prediction 1: Most change in fire from spinup to modern OR
# Test: compare number of fires over 150 year interval; compare number of fires during first/last 25,
# comparing BETWEEN time slices. 
  
  # Need to find fire regimes for spinup
  dat.spinup <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_FRI.spinfinish.v5.csv")
  
  # Set up datatable so I can average by SM_FIRE
  RUNID <- as.character(dat.spinup$RUNID) # Stores RUNID as a character
  factors <- t(data.frame(strsplit(RUNID, split="-f")))
  rownames(factors) <- c() # Gets rid of the row names that show up.
  colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
  factors <- data.frame(factors)
  dat.spinup <- cbind(factors,dat.spinup) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
  dat.spinup$SLXSAND <- car::recode(dat.spinup$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
  dat.spinup$SM_FIRE <- car::recode(dat.spinup$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
  dat.spinup$SM_FIRE <- factor(dat.spinup$SM_FIRE, levels=c(0, 0.01, 0.02, 0.0))
  dat.spinup$FRI <- car::recode(dat.spinup$FRI, "'Inf'='100'")
  
  dat.sd <- aggregate(dat.spinup[c("FRI")], by=dat.spinup[c("SM_FIRE")], FUN=sd)
  colnames(dat.sd) <- c("SM_FIRE","FRI.sd")
  dat.spinup <- aggregate(dat.spinup[c("FRI")], by=dat.spinup[c("SM_FIRE")], FUN=mean)
  dat.spinup <- merge(dat.spinup, dat.sd)
  rm(dat.sd)
  dat.spinup <- dat.spinup[,c("SM_FIRE","FRI","FRI.sd")]
  dat.spinup$Phase <- "spinup"
  
  dat.compare <- dat.spinup[c("SM_FIRE","FRI")]
  colnames(dat.compare) <- c("SM_FIRE","FRI.spinup")
  
  # Have fire regimes for this table from dat.regime
  dat.temp <- subset(dat.agb, subset=dat.agb$year>=max(dat.agb$year)-100)
  lapse <- max(dat.temp$year)-min(dat.temp$year)
  lapse
  dat.temp <- aggregate(dat.all[c("fire")], by=dat.all[c("RUNID","SLXSAND","SM_FIRE")], FUN=sum) # Find fire regime
  dat.temp$FRI <- lapse/dat.temp$fire # Calculates the fire return interval by divinding the number of fires per scenario by the number of fires
  dat.temp$FRI <- car::recode(dat.temp$FRI, "'Inf'='100'")
  dat.sd <- aggregate(dat.temp[c("FRI")], by=dat.temp[c("SM_FIRE")], FUN=sd)
  colnames(dat.sd) <- c("SM_FIRE","FRI.sd")
  dat.temp <- aggregate(dat.temp[c("FRI")], by=dat.temp[c("SM_FIRE")], FUN=mean)
  dat.temp <- merge(dat.temp, dat.sd)
  dat.temp$Phase <- "run"
  dat.spinup <- rbind(dat.spinup, dat.temp)
  
  dat.compare$FRI.run <- dat.temp$FRI
  rm(dat.temp)
  dat.compare$FRI.diff <- dat.compare$FRI.spinup - dat.compare$FRI.run
  
    # ------------------------------------------------------------------------------
    # Figure to compare fire frequency between spinup and runs across fire scenarios
    # ------------------------------------------------------------------------------
    
    library(ggplot2)
    pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/change_in_FRI")
    ggplot(dat.spinup, aes(x = SM_FIRE, y = FRI, fill = Phase)) + 
      geom_bar(stat="identity", position="dodge") +
      geom_errorbar(aes(ymin = FRI - FRI.sd, ymax = FRI + FRI.sd, width=0.1), position = position_dodge(0.9)) +
      theme_bw() +  
      xlab("Fire Threshold") + 
      ylab("Fire Return Interval (years)") + 
      ggtitle("Fire Return Intervals:\nComparing Spinup and Runs")
    dev.off()

  # ---------------------------------------------------------------------------------
  # Graph differences in fire frequency between spinup and runs across fire scenarios
  # ----------------------------------------------------------------------------------
  
  dat.spinup <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_FRI.spinfinish.v5.csv")
  dat.spinup$FRI <-car::recode(dat.spinup$FRI, "'Inf'='100'")
  
  dat.temp <- subset(dat.agb, subset=dat.agb$year>=max(dat.agb$year)-100)
  dat.regime <- aggregate(dat.temp[c("fire")], by=dat.temp[c("RUNID","SM_FIRE","SLXSAND")], FUN = sum) # Find fire regime
  lapse <- max(dat.temp$year)-min(dat.temp$year)
  dat.regime$FRI.runs <- lapse/dat.regime$fire # Calculates the fire return interval by divinding the number of fires per scenario by the number of fires
  dat.regime$FRI.runs <-car::recode(dat.regime$FRI, "'Inf'='100'")
  dat.regime <- dat.regime[,c("RUNID","SLXSAND","SM_FIRE","FRI.runs")]
  
  dat.FRI <- merge(dat.spinup, dat.regime)
  dat.FRI$FRI.diff <- dat.FRI$FRI.runs-dat.FRI$FRI
  dat.FRI_compare <- aggregate(dat.FRI[c("FRI.diff")], by = dat.FRI[c("SM_FIRE")], FUN = mean)
  dat.sd <- aggregate(dat.FRI[c("FRI.diff")], by = dat.FRI[c("SM_FIRE")], FUN = sd)
  colnames(dat.sd) <- c("SM_FIRE","FRI.diff.sd")
  dat.FRI_compare <- merge(dat.FRI_compare, dat.sd)
  
  # To understand the pattern in the data
  subset(dat.FRI, subset=dat.FRI$FRI.diff==max(dat.FRI$FRI.diff))
  
  dat.sd <- aggregate(dat.FRI[c("FRI","FRI.runs")], by=dat.FRI[c("SM_FIRE")], FUN=sd)
  colnames(dat.sd) <- c("SM_FIRE","FRI.sd","FRI.runs.sd")
  dat.FRI_means <- aggregate(dat.FRI[c("FRI","FRI.runs")], by = dat.FRI[c("SM_FIRE")], FUN=mean)
  dat.FRI_means <- merge(dat.FRI_means, dat.sd)
  
  dat.FRI_spinup <- dat.FRI_means[c("SM_FIRE","FRI", "FRI.sd")]
  dat.FRI_spinup$slice <- "Initial State"
  colnames(dat.FRI_spinup) <- c("SM_FIRE","FRI","sd","slice")
  dat.FRI_runs <- dat.FRI_means[c("SM_FIRE","FRI.runs","FRI.runs.sd")]
  dat.FRI_runs$slice <- "Final State"
  colnames(dat.FRI_runs) <- c("SM_FIRE","FRI","sd","slice")
  dat.FRI_test <- rbind(dat.FRI_spinup, dat.FRI_runs)
  dat.FRI_test$slice <- factor(dat.FRI_test$slice, levels=c("Initial State","Final State"))
  
  ######
  # Graph for poster
  #######
  
  pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/change_in_FRI")
  ggplot(dat.FRI_test, aes(x = SM_FIRE, y = FRI, fill = slice)) + 
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = FRI - sd, ymax = FRI + sd, width=0.3), position = position_dodge(0.9)) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 55, margin = margin(t=20)),
           axis.text.y = element_text(size = 55, margin = margin(r=20)),
           axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
           axis.title.y = element_text(size = 60, face = "bold", margin = margin(t = 50, r = 20)),
           legend.title = element_text(size=60),
           legend.text = element_text(size=55, margin = margin(t = 20)),
           legend.key.size = unit(3, "line")) +
    scale_fill_manual(name = "Time Frame", values = c("orange","olivedrab4")) +
    xlab("Fire Threshold") + 
    ylab("Fire Return Interval (years)")
    # ggtitle("Fire Return Intervals:\nComparing Spinup and Runs")
  dev.off()
  
  # Graph 
  pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/FRI_difference")
  ggplot(dat.FRI_compare, aes(x = SM_FIRE, y = FRI.diff)) + 
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = FRI.diff - FRI.diff.sd, ymax = FRI.diff + FRI.diff.sd, width=0.1)) +
    theme_bw() +  
    xlab("Fire Threshold") + 
    ylab("Difference in Fire Return Interval (years)") + 
    ggtitle("Change in Fire Return Interval\nBetween Spinups and Runs")
  dev.off()
  
# Prediction 2: Greatest change in mean soil moisture from spinup to end
# Test: compare mean soil moisture from first 25 years to mean soil moisture from last 25 years; 
# compare BETWEEN time slice
  
dat.soil <- subset(dat.all, subset = dat.all$pft=="Hardwoods")
dat.soil <- dat.soil[,c("RUNID","SLXSAND","SM_FIRE","year","w.agb","soil_moist","fire")]
dat.soil_first <- subset(dat.soil, subset = dat.soil$year<=25)
dat.sd <- aggregate(dat.soil_first[c("soil_moist")], by = dat.soil_first[c("RUNID","SLXSAND","SM_FIRE")], FUN = sd)
colnames(dat.sd) <- c("RUNID","SLXSAND","SM_FIRE","soil_moist.sd")
dat.soil_first <- aggregate(dat.soil_first[c("soil_moist")], by = dat.soil_first[c("RUNID","SLXSAND","SM_FIRE")], FUN = mean)
dat.soil_first <- merge(dat.soil_first, dat.sd)

dat.soil_last <- subset(dat.soil, subset = dat.soil$year>=max(dat.soil$year)-25)
dat.sd <- aggregate(dat.soil_last[c("soil_moist")], by = dat.soil_last[c("RUNID","SLXSAND","SM_FIRE")], FUN = sd)
colnames(dat.sd) <- c("RUNID","SLXSAND","SM_FIRE","soil_moist.sd")
dat.soil_last <- aggregate(dat.soil_last[c("soil_moist")], by = dat.soil_last[c("RUNID","SLXSAND","SM_FIRE")], FUN = mean)
dat.soil_last <- merge(dat.soil_last, dat.sd)

  # -----------------------------------------------------------------
  # Figure examining average change in soil moisture across different sands  
  # -----------------------------------------------------------------
  # KEY FIGURE FOR PAPER

dat.soil_last$slice <- "Last 25 Years"
dat.soil_first$slice <- "First 25 Years"
dat.soil <- rbind(dat.soil_first, dat.soil_last)
dat.sd <- aggregate(dat.soil[c("soil_moist")], by = dat.soil[c("SLXSAND","slice")], FUN=sd)
colnames(dat.sd) <- c("SLXSAND","slice","soil_moist.sd")
dat.soil <- aggregate(dat.soil[c("soil_moist")], by = dat.soil[c("SLXSAND","slice")], FUN=mean)
dat.soil <- merge(dat.soil, dat.sd)
dat.soil$slice <- factor(dat.soil$slice, levels=c("First 25 Years","Last 25 Years"))


library(ggplot2)
# pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/soilmoist_diff")
ggplot(dat.soil, aes(x = SLXSAND, y=soil_moist, fill = slice)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = soil_moist - soil_moist.sd, ymax = soil_moist + soil_moist.sd, width = 0.3), position = position_dodge(0.9)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 55, margin = margin(t=20)),
        axis.text.y = element_text(size = 55, margin = margin(r=20)),
        axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 60, face = "bold", margin = margin(t = 50, r = 20)),
        legend.title = element_text(size=60),
        legend.text = element_text(size=55, margin = margin(t = 20)),
        legend.key.size = unit(3, "line")) +
  scale_fill_manual(name = "Time Slice", values = c("gold3","olivedrab3")) +
  xlab("Time Slice") + 
  ylab (expression(bold(paste("Soil Moisture (kg", " m"^"-2",")")))) 
  # ggtitle("Change in Soil Moisture")
dev.off()

#######
       
colnames(dat.soil_first) <- c("RUNID","SLXSAND","SM_FIRE","soilmoist_first","soilmoist_first.sd")
colnames(dat.soil_last) <- c("RUNID","SLXSAND","SM_FIRE","soilmoist_last","soilmoist_last.sd")
dat.soil <- merge(dat.soil_first, dat.soil_last)
dat.soil$soilmoist.diff <- dat.soil$soilmoist_last - dat.soil$soilmoist_first
dat.sd <- aggregate(dat.soil[c("soilmoist.diff")], by = dat.soil[c("SM_FIRE")], FUN = sd)
colnames(dat.sd) <- c("SM_FIRE","sd")
dat.soil <- aggregate(dat.soil[c("soilmoist.diff")], by = dat.soil[c("SM_FIRE")], FUN = mean)
dat.soil <- merge(dat.soil, dat.sd)

pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/soilmoist_diff_fire_thresholds")
ggplot(dat.soil, aes(x = SM_FIRE, y = soilmoist.diff)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = soilmoist.diff - sd, ymax = soilmoist.diff + sd, width = 0.1)) + 
  theme_bw() + 
  xlab("Fire Threshold") + 
  ylab (expression(bold(paste("Difference in Mean Soil Moisture\nbetween First and Last 25 Years (kg", " m"^"-2",")")))) + 
  ggtitle("Change in Soil Moisture\nbetween First and Last 25 Years")
dev.off()
  