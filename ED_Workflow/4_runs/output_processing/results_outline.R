# Clean script to work with for the results
# All dataframes will be labeled as dat.name, where the name somewhat describes the content. 

# ############################################################
# Prepare datatable that will be used for statistical analysis
# ############################################################

# Load all data and format datatable. Note that filepaths will change between computers. 
# path.google <- "/Volumes/GoogleDrive/My Drive/URF 2018 Butkiewicz" # Filepath for Christy when she wants to run the script herself. 
path.google <- "/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/"
# dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")
dat.all <- read.csv(file.path(path.google, "v5_tables", "output_runs.v5.csv")) # Filepath for Christy when she wants to run the script herself. 
summary(dat.all) 
# When reading the datatable: 
# RUNID = the identity of the treatment. Coded so that the first half (s#) corresponds to a soil texture and the second half (f#) corresponds to a fire setting. 
# year = from 0 to 214. 0 corresponds to the year 1800 CE and 214 to 2014 CE.
# yearCE = calendar year, CE represents the "CE" or "Common Era" tag. 
# w.agb = Aboveground Biomass (kg C m-2)
# w.dens = Density (plants m-2)
# w.dbh = Diameter at Breast Height (cm)
# w.ba = Basal Area (cm2 m-2)
# w.dens.tree = Density of Trees with DBH > 10 cm (trees m-2)
# w.dbh.tree = Diameter at Breast Height of Trees with DBH > 10 cm (cm2 m-2)
# w.ba.tree = Basal Area of Trees with DBH > 10 cm (cm2 m-2)
# soil_moist = Soil Moisture, proportion of saturation
# fire = Binary variable describing whether or not fire occured in that year of the simulation. 0 = no fire occurred, 1 = fire occured. 

# Add actual years
dat.all$year <- dat.all$year+1800

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

# Possible changes to soil moisture to convert from m3 H2O m-3 to kg H2O m-3
dat.all$soil_moist <- dat.all$soil_moist*1000 # Density of water is 1000 kg m-3, so this converts m3 H2O to kg H2O 

# Prepare datatable that will be used for analyzing aboveground biomass
dat.analy <- aggregate(dat.all[,c("w.agb")], by=dat.all[,c("SLXSAND","SM_FIRE","RUNID","year","fire")], FUN=sum, na.rm = T) # Sum agb across pfts for each RUNID and year
colnames(dat.analy) <- c("SLXSAND","SM_FIRE","RUNID","year","fire","agb") # Aggregate gives you a column labeled "x" for some reason, so here we change it to agb
dat.fire <- dat.analy[,c("SLXSAND","SM_FIRE","RUNID","year","fire")]

# Find change in aboveground biomass

# Subset aboveground biomass for the first 25 years of each simulation. 
dat.tmp1 <- subset(dat.analy, subset = dat.analy$year<min(dat.analy$year)+25) # Subset first 25 years of the simulation
  length(dat.tmp1$year)/25 # Check to make sure that the subset worked, divide by 25 for 25 runs
  range(dat.tmp1$year)
dat.sd <- aggregate(dat.tmp1["agb"], by = dat.tmp1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd") # agb_1 refers to mean aboveground biomass for the first 25 years of the simulation. 
dat.tmp1 <- aggregate(dat.tmp1["agb"], by = dat.tmp1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
dat.tmp1 <- merge(dat.tmp1, dat.sd)
colnames(dat.tmp1) <- c("SLXSAND","SM_FIRE","RUNID","agb_1","agb_1.sd")

# Subset aboveground biomass for the last 25 years of each simulation
dat.tmp2 <- subset(dat.analy, subset = dat.analy$year>max(dat.analy$year)-25)
  length(dat.tmp2$year)/25 # Check to make sure that the subset worked, divide by 25 for 25 runs
  range(dat.tmp2$year)
dat.sd <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sd)
colnames(dat.sd) <- c("SLXSAND","SM_FIRE","RUNID","sd")
dat.tmp2 <- aggregate(dat.tmp2["agb"], by = dat.tmp2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
dat.tmp2 <- merge(dat.tmp2, dat.sd)
colnames(dat.tmp2) <- c("SLXSAND","SM_FIRE","RUNID","agb_L","agb_L.sd")

dat.analy <- merge(dat.tmp1, dat.tmp2)
rm(dat.tmp1, dat.tmp2, dat.sd) # Remove unnecessary variables
dat.analy$diff <- dat.analy$agb_L - dat.analy$agb_1
dat.analy$p.diff <- dat.analy$diff / dat.analy$agb_1 # p.diff stands for proportional difference

# Derive soil moisture

dat.sm <- aggregate(dat.all[c("soil_moist")], by = dat.all[,c("SLXSAND","SM_FIRE","RUNID","year")], FUN = mean) # datatable referred to as dat.sm for "soil moisture data"

dat.sm1 <- subset(dat.sm, subset = dat.sm$year<min(dat.sm$year)+25)
length(dat.sm1$year)/25
dat.sm1 <- aggregate(dat.sm1["soil_moist"], by = dat.sm1[c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
colnames(dat.sm1) <- c("SLXSAND","SM_FIRE","RUNID","sm.1")
dat.smL <- subset(dat.sm, subset = dat.sm$year>max(dat.sm$year)-25)
length(dat.smL$year)/25
dat.smL <- aggregate(dat.smL["soil_moist"], by = dat.smL[c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
colnames(dat.smL) <- c("SLXSAND","SM_FIRE","RUNID","sm.L")

dat.sm <- merge(dat.sm1, dat.smL)
dat.sm$sm.diff <- dat.sm$sm.L - dat.sm$sm.1

dat.analy <- merge(dat.sm, dat.analy)
rm(dat.sm, dat.sm1, dat.smL) # Remove unnecessary variables

# Derive fire regimes

# Derive total number of fires to occur during run
dat.regime <- aggregate(dat.fire["fire"], by = dat.fire[c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
colnames(dat.regime)
colnames(dat.regime) <- c("SLXSAND","SM_FIRE","RUNID","nfire") # Rename columns. Here nfire means "number of fires"

# Derive change in number of fires through 100 year window
# First look at first 100 year window (1800-1900)
dat.regime1 <- subset(dat.fire, subset = dat.fire$year<min(dat.fire$year)+100)
  length(unique(dat.regime1$year))
  range(dat.regime1$year)
dat.regime1 <- aggregate(dat.regime1["fire"], by = dat.regime1[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
colnames(dat.regime1) <- c("SLXSAND","SM_FIRE","RUNID","fire.1")

# Derive number of fires during last 100 year window (1914-2014)
dat.regime2 <- subset(dat.fire, subset = dat.fire$year>max(dat.fire$year)-100)
  length(unique(dat.regime2$year))
  range(dat.regime2$year)
dat.regime2 <- aggregate(dat.regime2["fire"], by = dat.regime2[,c("SLXSAND","SM_FIRE","RUNID")], FUN = sum)
colnames(dat.regime2) <- c("SLXSAND","SM_FIRE","RUNID","fire.L")
dat.regime1 <- merge(dat.regime1, dat.regime2)
dat.regime1$diff <- dat.regime1$fire.L - dat.regime1$fire.1
dat.regime1$p.diff <- dat.regime1$diff / dat.regime1$fire.1
dat.regime1$p.diff <- car::recode(dat.regime1$p.diff, "'NaN'='0'")
dat.regime <- merge(dat.regime, dat.regime1)
colnames(dat.regime)
colnames(dat.regime) <- c("SLXSAND","SM_FIRE","RUNID","nfire","nfire.1","nfire.L","nfire.diff","nfire.pdiff")
dat.analy <- merge(dat.regime, dat.analy)
rm(dat.fire, dat.regime, dat.regime1, dat.regime2) # Remove unnecessary variables

# README for dat.analy: 
  # SLXSAND refers to the proportion of sand present in the soil. The rest is filled with clay. So a SLXSAND
  # value of 0.38 means that the soil is 38% sand and 62% clay. 
  # SM_FIRE refers to the flammability of the ecosystem. Higher SM_FIRE values (up to 0.04) mean that the
  # ecosystem is more flammable. Lower values (down to 0.00) mean that the ecosystem is less flammable.
  # RUNID refers to the identity of the run. 
  # nfire refers to the total number of times the ecosystem caught fire between 1800 and 2014. 
  # nfire.1 refers to how many times the ecosytem caught fire in the first 100 years of the simulation (1800- 1899)
  # nfire.L refers to the number of times the ecosystem caught fire in the last 100 years of the simulation (1915-2014)
  # nfire.diff is the change in fire occurrence (number of times the ecosystem caught fire) between the 
  # first and last 100 years of the simulation. 
  # nfire.pdiff is the proportional change in fire occurrence between the first and last 100 years of the simulation. 
  # sm.1 refers to the average soil moisture over the first 25 years of the simulation. 
  # sm.L refers to the average soil moisture over the last 25 years of the simulation. 
  # sm.diff refers to the change in average soil moisture between the first and last 25 years of the simulation. 
  # agb_1 refers to the average aboveground biomass over the first 25 years of the simulation. 
  # agb_L refers to the average aboveground biomass over the last 25 years of the simulation. 
  # diff refers to the absolute change in average aboveground biomass between the first and last 25 years of the simulation. 
  # p.diff refers to the proportional change in average aboveground biomass between the first and last 25 years of the simulation. 

###################

# ##################
# Summary Statistics
# ##################

print(paste0("Average Change in Aboveground Biomass: ",round(mean(dat.analy$p.diff)*100, digits = -1)," ± ",round(sd(dat.analy$p.diff)*100, digits = -1)," %")) # I rounded it to reporting conventions with only one significant figure in the standard deviation. 

print(paste0("Average Aboveground Biomass across First 25 Years: ",round(mean(dat.analy$agb_1), digits = 1)," ± ",round(sd(dat.analy$agb_1), digits = 1)," kg C m-2"))

print(paste0("Average Aboveground Biomass across First 25 Years: ",round(mean(dat.analy$agb_L), digits = 1)," ± ",round(sd(dat.analy$agb_L), digits = 1)," kg C m-2"))

print(paste0("Average Change in Soil Moisture: ",round(mean(dat.analy$sm.diff)*100, digits = 1)," ± ",round(sd(dat.analy$sm.diff)*100, digits = 1)," % saturation"))

# Summary statistics for fire
range(dat.analy$nfire) # Range of number of fires throughout scenario
subset(dat.analy, subset = dat.analy$nfire == max(dat.analy$nfire)) # Returns which scenario caught fire the most
print(paste0("Number of Scenarios that never caught fire: ",nrow(subset(dat.analy, subset = dat.analy$nfire == min(dat.analy$nfire)))))

# #################
# Statistical Tests
# #################

# Make treatments continuous variables
summary(dat.analy)
dat.analy$SM_FIRE <- as.numeric(as.character(dat.analy$SM_FIRE)) # Make parameters continuous
dat.analy$SLXSAND <- as.numeric(as.character(dat.analy$SLXSAND)) # Make parameters continuous

# Effects of treatments on change in biomass
treatment.lm <- lm(p.diff ~ sm.1*nfire, data = dat.analy)
summary(treatment.lm) # Soil significantly affects proportional changes in agb, p = 0.02, fire does not affect proportional change in agb (p = 0.2), and there is no interaction between the two variables (p = 0.4092)
range(dat.analy$sm.1)
  max(dat.analy$sm.1) - min(dat.analy$sm.1)
range(dat.analy$nfire)

# Because treatment.lm says there is no signficant interaction between initial soi moisture & total number of fires, I ran a second statistical test that partitioned the variance between those two variables separately
treatment.lm2 <- lm(p.diff ~ sm.1 + nfire, data = dat.analy) 
summary(treatment.lm2) # Soil and fire both significantly affect proportional change in agb, p < 0.01
# All significant because 

# Some notes: the output should have the (Intercept) at ~ 1.77. This means that the average ecosystem, regardless of soil moisture and number of fires, will increase by 177%. That's probably due to Carbon and other effects of climate. The slope of sm.1 should be around 3.53, significantly different from 0. Nfire has a slope of 0.01, which is not significantly different from 0. Combining soil moisture and number of fires has a slope of 0.03, which is not significantly different from 0.
# List t*, n, and p value) 

change.lm <- lm(p.diff ~ sm.diff*nfire.diff, data = dat.analy)
summary(change.lm)

# Because change.lm says there is no signficant interaction between change in soi moisture & change in fire, I ran a second statistical test that partitioned the variance between those two variables separately
change.lm2 <- lm(p.diff ~ sm.diff + nfire.diff, data = dat.analy)
summary(change.lm2)

range(dat.analy$sm.diff)
  max(dat.analy$sm.diff) - min(dat.analy$sm.diff)
range(dat.analy$nfire.diff)

range(dat.analy$p.diff)
  max(dat.analy$p.diff) - min(dat.analy$p.diff)

# #######################
# Figures for Publication
# #######################

# Load necessary package
library(ggplot2)

# Figure 1
  # ---------
  # Summarizes aboveground biomass during the first and last 25 years of the simulation
  
  # Prepare dataframe
  dat.fig1 <- rbind(data.frame(dat.analy[,c("SLXSAND","SM_FIRE","RUNID")],
                               agb = dat.analy$agb_1,
                               sd = dat.analy$agb_1.sd,
                               time_slice = "First 25 Years"),
                    data.frame(dat.analy[,c("SLXSAND","SM_FIRE","RUNID")],
                               agb = dat.analy$agb_L,
                               sd = dat.analy$agb_L.sd,
                               time_slice = "Last 25 Years"))
  
  # pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/agb_all_runs.pdf")
  ggplot(dat.fig1, aes(x = SM_FIRE, y = agb, fill=SLXSAND)) + 
    facet_grid(time_slice ~ .) + 
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = agb - sd, ymax = agb + sd, width=0.1), position = position_dodge(0.9)) + 
    theme_bw() + 
    scale_fill_manual(name = "Sand\nFraction", values = c("olivedrab4","olivedrab3","lightgoldenrod3","gold3","orange")) +
    # ggtitle("Aboveground")+ 
    xlab("Fire Threshold") + 
    ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")"))))
  # dev.off()
  # ---------
  
  # Figure 2a
  # --------
  # Demonstrates results of 
  
  ggplot(dat.analy, aes(x = sm.1, y = p.diff)) + 
    geom_point() +
    theme_bw() + 
    stat_smooth(method = "lm", col = "orange") + 
    xlab("Soil Moisture") + 
    ylab (expression(bold(paste("Proportional Change in Aboveground Biomass"))))
  
  # -------
  
  # Figure 2b
  # --------
  
  nfire.lm <- lm(p.diff ~ nfire, data = dat.analy)
  summary(nfire.lm)
  ggplot(dat.analy, aes(x = nfire, y = p.diff)) + 
    geom_point() +
    theme_bw() + 
    stat_smooth(method = "lm", col = "orange") + 
    xlab("Total Number of Fires") + 
    ylab (expression(bold(paste("Proportional Change in Aboveground Biomass"))))
  # ---------
  
  # Figure 3a
  # ---------
  
  ggplot(dat.analy, aes(x = sm.diff, y = p.diff)) + 
    # facet_grid(time_slice ~ .) + 
    geom_point() +
    # geom_errorbar(aes(ymin = agb - sd, ymax = agb + sd, width=0.1), position = position_dodge(0.9)) + 
    theme_bw() + 
    # scale_fill_manual(name = "Sand\nFraction", values = c("olivedrab4","olivedrab3","lightgoldenrod3","gold3","orange")) +
    # ggtitle("Aboveground")+ 
    stat_smooth(method = "lm", col = "orange") + 
    xlab("Change in Soil Moisture") + 
    ylab (expression(bold(paste("Proportional Change in Aboveground Biomass"))))
  # ---------
  
  # Figure 3b
  # ---------
  ggplot(dat.analy, aes(x = nfire.diff, y = p.diff)) + 
    # facet_grid(time_slice ~ .) + 
    geom_point() +
    # geom_errorbar(aes(ymin = agb - sd, ymax = agb + sd, width=0.1), position = position_dodge(0.9)) + 
    theme_bw() + 
    # scale_fill_manual(name = "Sand\nFraction", values = c("olivedrab4","olivedrab3","lightgoldenrod3","gold3","orange")) +
    # ggtitle("Aboveground")+ 
    stat_smooth(method = "lm", col = "orange") + 
    xlab("Change in Number of Fires") + 
    ylab (expression(bold(paste("Proportional Change in Aboveground Biomass"))))
  # --------