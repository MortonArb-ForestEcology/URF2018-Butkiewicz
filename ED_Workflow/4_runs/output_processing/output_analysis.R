################################
# OUTPUT ANALYSIS, Foward Runs #
################################

# This file is currently a rough outline of what I want this script to do. Comments will only be understandable to the author. 

# Load necessary packages into the environment
library(car)
library(ggplot2)

# Load necessary data into the environment
dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")

# Add Fire Regimes to data table
dat.regime <- aggregate(dat.all[c("fire")], by=dat.all[c("RUNID","pft")], FUN=sum) # Find fire regime
dat.regime <- subset(dat.regime, subset=dat.regime$pft=="Hardwoods") # Remove redundancies
dat.regime <- dat.regime[,c("RUNID","fire")] # Remove unnecessary pft column
colnames(dat.regime) <- c("RUNID","nfire") # Rename columns. Here nfire means "number of fires"
summary(dat.regime)

# Prepare table to graph
dat.all <- dat.all[,1:4] # Subset original datatable to only include agb so that the data is reasonable
dat.all <- aggregate(dat.all[,"w.agb"], by=dat.all[,c("RUNID","year")], FUN=sum, na.rm=T)# Sum agb for each RUNID
colnames(dat.all) <- c("RUNID","year","agb") # Make names reasonable

# Get data for differences between first and last 25 years of data
dat.first <- subset(dat.all, subset=dat.all$year <= 24) # Subset so that only the first few years (from 0-24) are taken
dat.first$chunk <- "First 25 Years" # Add label so we can sort in ggplot later
dat.last <- subset(dat.all, subset=dat.all$year >= 190) # Subset so that only the last 25 years (from 190-214) are taken
dat.last$chunk <- "Last 25 Years" # Add label so we can sort in ggplot later
dat.graph <- rbind(dat.first, dat.last)
dat.graph <- merge(dat.graph, dat.regime) # Add fire regime to the table
summary(dat.graph)
dat.graph$nfire <- as.character(dat.graph$nfire)


# ############################### # 
# Box-and-whisker plot for graphs #
# #################################

ggplot(dat.graph, aes(x = nfire, y = agb, fill = chunk)) + 
  geom_boxplot(position = position_dodge(width=1), lwd=0.7) 

########

# Make a separate data table with the differences in agb between firs tand last 25 years
dat.analy <- aggregate(dat.first[c("w.agb")], by=dat.first[c("nfire","RUNID")], FUN=mean)
colnames(dat.analy) <- c("nfire","RUNID","agb.First_25_Years")
dat.tmp <- aggregate(dat.last[c("w.agb")], by=dat.last[c("nfire","RUNID")], FUN=mean)
colnames(dat.tmp) <- c("nfire","RUNID","agb.Last_25_Years")
dat.analy <- merge(dat.analy, dat.tmp)
rm(dat.tmp, dat.first, dat.last) # Clean up environment
dat.analy$difference <- dat.analy$agb.Last_25_Years - dat.analy$agb.First_25_Years

# Prepare datatable for graph of differences
dat.graph <- aggregate(dat.analy[c("difference")], by=dat.analy[c("nfire")], FUN=mean)
dat.sd <- aggregate(dat.analy[c("difference")], by=dat.analy[c("nfire")], FUN=sd)
dat.graph$sd <- dat.sd$difference
rm(dat.sd)

sort(unique(dat.regime$nfire), decreasing = FALSE) # Provides list of fire regimes

# ################################################ #
# Graph for differences in agb due to fire regime
# ##################################################

ggplot(dat.graph, aes(nfire, difference)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=difference-sd, ymax=difference+sd), width=0.2, position=position_dodge(.9)) +
  xlab("Number of Fires") + 
  ylab("Difference in AGB") 

ggplot(dat.graph, aes(nfire, difference)) + 
  geom_line() + 
  xlab("Number of Fires") + 
  ylab("Difference in AGB") 
