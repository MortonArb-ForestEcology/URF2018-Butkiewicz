#
# SUMMARY STATISTICS
# 

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

# Summary statistics for control group
dat.subset <- subset(dat.all, subset=dat.all$SM_FIRE==0)
range.control <- range(dat.subset$p.ba.tree)

dat.control <- aggregate(x=dat.subset[,c("p.ba.tree")],
                     by=dat.subset[,c("RUNID","SLXSAND","SM_FIRE")],
                     FUN=mean)
colnames(dat.control) <- c("RUNID","SLXSAND","SM_FIRE","mean.ba")
min_mean <- min(dat.control$mean.ba)
min_mean <- subset(dat.control, subset=dat.agg$mean.ba==min_mean)
min_mean
max_mean <- max(dat.control$mean.ba)

# Summary statistics for experimental group (SM_FIRE=0)
dat.subset <- subset(dat.all, subset=dat.all$SM_FIRE==0.04)
range.exp <- range(dat.subset$p.ba.tree)

dat.exp <- aggregate(x=dat.subset[,c("p.ba.tree")],
                         by=dat.subset[,c("RUNID","SLXSAND","SM_FIRE")],
                         FUN=mean)
colnames(dat.exp) <- c("RUNID","SLXSAND","SM_FIRE","mean.ba")
min_mean <- min(dat.exp$mean.ba)
min_mean <- subset(dat.exp, subset=dat.exp$mean.ba==min_mean)
min_mean
min_mean <- max(dat.exp$mean.ba)
max_mean <- subset(dat.exp, subset=dat.exp$mean.ba==min_mean)
max_mean

# summary statistis for sandy soil (93% sand)
dat.subset <- subset(dat.all, subset=dat.all$SLXSAND==0.93)
range.sand <- range(dat.subset$p.ba.tree)
ba.min <- min(dat.subset$p.ba.tree)
ba.min <- subset(dat.subset, subset=dat.subset$p.ba.tree==ba.min)
ba.max <- max(dat.subset$p.ba.tree)
ba.max <- subset(dat.subset, subset=dat.subset$p.ba.tree==ba.max)

