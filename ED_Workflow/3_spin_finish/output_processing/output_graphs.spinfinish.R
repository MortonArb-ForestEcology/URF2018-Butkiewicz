#######################
# Graphing the Output #
#######################

# This is not meant to be run on the GitHub repository, but instead provides an example of how I generated the graphs in my results.

library(ggplot2)
library(car)

# Get the data into the environment: 
# files.output <- file.path("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/")
FRI <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_FRI_v4.csv")
FRI$FRI <- car::recode(FRI$FRI, "'Inf'='100'")
dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_runs_v4.csv") #Reads in dataframe with pft, agb, density, dbh, basal area, etc. 

# Modify the data in whatever way I think will make graphing easier:
dat.all <- merge(dat.all, FRI)
RUNID <- as.character(dat.all$RUNID) 
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.all$SLXSAND <- car::recode(dat.all$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.all$SM_FIRE <- car::recode(dat.all$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")

# Aggregate data down all 101 years to make 
dat.all.agg <- aggregate(x=dat.all[,c("p.dens","p.dens.tree","p.ba","soil_moist","p.agb")],
                         by=dat.all[,c("RUNID","SLXSAND","SM_FIRE")],
                         FUN=mean)
dat.all.agg <- merge(dat.all.agg, FRI)

#------------------------------
# STANDARDIZED SOIL GRAPHS
### Lines 34-_____ create a set of standardized soil moisture values and a graph. Because the soil moisture varies so much with soil texture, determining whether it depends on the fire frequency requires that the values be compared against our control scenario: s5, which had fire turned off. 
#------------------------------

soil <- c(0.38, 0.52, 0.66, 0.8, 0.93)
sm.calc <- matrix(0, ncol=5, nrow=length(soil)) #make a matrix to put things into where the rows will correspond to the soil
for(s in 1:length(soil)){
  temp <- dat.all.agg[which(dat.all.agg$SLXSAND==soil[s]),] #make temporary vector with soil moisture
  temp <- temp$soil_moist #get rid of unnecessary columns
  sm.calc[s,] <- temp #fill soil moisture values into row
}
data.frame(sm.calc)
rownames(sm.calc) <- soil
colnames(sm.calc) <- c("f5","f4","f3","f2","f1")

sm.standard <- matrix(0, ncol=4, nrow=length(soil)) #make another matrix to put the standardized values into
rownames(sm.standard) <- soil
for(i in 2:ncol(sm.calc)){
  sm.standard[,i-1] <- sm.calc[,i] - sm.calc[,5]
} #This loop might be just a little messed up. I need to lay with it some more. 
# rm(sm.calc)
sm.standard <- data.frame(sm.standard) #make it into a dataframe
colnames(sm.standard) <- c("0.04","0.03","0.02","0.01") #change the colnames to something reasonable
sm.standard <- stack(sm.standard) #stack them so that it's easier to graph
sm.standard$SLXSAND <- rep(soil) #
colnames(sm.standard) <- c("soilmoist","SM_FIRE","SLXSAND") #
RUNID.2 <- dat.all.agg[,1:3]
sm.standard <- merge(RUNID.2, sm.standard)
sm.standard <- merge(sm.standard,FRI)

# --------
# OTHER GRAPHS
# ---------

# Standardized Soil Moisture vs. FRI
# pdf("/Users/Cori/Desktop/graph")
ggplot(sm.standard, aes(x=FRI, y=soilmoist)) + #Compares soil moisture against fire return interval to see if there's any covariation. 
  geom_point() + 
  facet_grid(. ~ SLXSAND) #Labels on the top represent proportion of sand in the soil. 
# dev.off()

# Density vs. Soil Moisture
ggplot(dat.all.agg, aes(x=SM_FIRE, y=p.dens.tree, color=SLXSAND)) + 
  geom_point()

# Density vs. FRI
ggplot(dat.all.agg, aes(x=as.numeric(paste(SLXSAND)), y=p.dens.tree, color=SM_FIRE)) + 
  geom_line() +
  geom_point()

ggplot(dat.all, aes(x=SLXSAND, y=p.dens.tree, fill=SM_FIRE)) + 
  geom_boxplot() 

ggplot(dat.all.agg, aes(x=SLXSAND, y=FRI, fill=SM_FIRE)) + 
  geom_bar()
