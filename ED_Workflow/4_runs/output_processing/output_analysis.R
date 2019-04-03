#################################
# OUTPUT ANALYSIS, Forward Runs #
#################################

# The first part of this analysis is meant to find patterns in the data. 
# ------------------------------------------------------------------------

dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")

# Add columns that individually specify soil and fire values 
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.all$SLXSAND <- car::recode(dat.all$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.all$SM_FIRE <- car::recode(dat.all$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

# Only look at agb because it is most commonly looked at in modeling papers
dat.agb <- dat.all[,c("SLXSAND","SM_FIRE","RUNID","year","pft","w.agb")] # Subset original datatable to only include agb so that the data is reasonable
dat.agb <- aggregate(dat.agb[,c("w.agb")], by=dat.agb[,c("SLXSAND","SM_FIRE","RUNID","year")], FUN=sum, na.rm = T) # Sum agb across pfts for each RUNID
colnames(dat.agb) <- c("SLXSAND","SM_FIRE","RUNID","year","agb") # Aggregate gives you a column labeled "x" for some reason, so I've changed it to agb

# This splits the data into two tables so that I can find the mean for the first 25 years and the mean for the last 25 years. 

  # Calculate the mean agb for the first 25 years of the simulation
  dat.first <- subset(dat.agb, subset = dat.agb$year<=min(dat.agb$year)+25) # Create dataframe with only first 25 years
  dat.first <- aggregate(dat.first["agb"], by = dat.first[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean) # Find mean for each soil texture
  colnames(dat.first) <- c("SLXSAND","SM_FIRE","RUNID","first_agb") # Rename columns to make merge in line 39 easier

  # Calculaate the mean agb for the last 25 years of the simulation
  dat.last <- subset(dat.agb, subset = dat.agb$year>=max(dat.agb$year)-25)
  dat.last <- aggregate(dat.last["agb"], by = dat.last[,c("SLXSAND","SM_FIRE","RUNID")], FUN = mean)
  colnames(dat.last) <- c("SLXSAND","SM_FIRE","RUNID","last_agb") # Rename columns to make merge in line 39 easier

# Make dataframe to analyze differences in soil and fire
dat.analy <- merge(dat.first,dat.last)
rm(dat.first, dat.last) # Remove unnecessary variables from the environment
dat.analy$difference <- dat.analy$last_agb - dat.analy$first_agb

# Preliminary look at some general patterns
subset(dat.analy, subset=dat.analy$difference==max(dat.analy$difference)) # Least stability?
subset(dat.analy, subset=dat.analy$difference==min(dat.analy$difference)) # Greatest stability?

# s3-f2 (66% sand, 0.03 fire threshold) --> most unstable
# s1-f1 (93% sand, 0.04 fire threshold) --> most stable

  ###################
  # Patterns in Soil
  ###################

  dat.soil <- aggregate(dat.analy["difference"], by=dat.analy["SLXSAND"], FUN=mean)
  dat.sd <- aggregate(dat.analy["difference"], by=dat.analy["SLXSAND"], FUN=sd)
  colnames(dat.sd) <- c("SLXSAND","sd") # Here sd stands for "standard deviation"
  dat.soil <- merge(dat.soil, dat.sd) # Include standard deviation
  rm(dat.sd) # Remove unnecessary variables
  
  # This next script is designed to help look for patterns in the data
  subset(dat.soil, subset=dat.soil$difference==max(dat.soil$difference)) # Least stability?
  subset(dat.soil, subset=dat.soil$difference==min(dat.soil$difference)) # Greatest stability?
  
  # 66% sand --> most unstable
  # 93% sand --> most stable
  
  ###################
  # Patterns in Fire
  ###################
  
  dat.fire <- aggregate(dat.analy["difference"], by=dat.analy["SM_FIRE"], FUN=mean)
  dat.sd <- aggregate(dat.analy["difference"], by=dat.analy["SM_FIRE"], FUN=sd)
  colnames(dat.sd) <- c("SM_FIRE","sd") # Here sd stands for "standard deviation"
  dat.fire <- merge(dat.fire, dat.sd) # Include standard deviation
  rm(dat.sd) # Remove unnecessary variables
  
  subset(dat.fire, subset=dat.fire$difference==max(dat.fire$difference)) # Least stability?
  subset(dat.fire, subset=dat.fire$difference==min(dat.fire$difference)) # Greatest stability?
  
  # Moderately able to catch fire (SM_FIRE = 0.02) --> least stable
  # Easily able to catch fire (SM_FIRE = 0.04) --> most stable
