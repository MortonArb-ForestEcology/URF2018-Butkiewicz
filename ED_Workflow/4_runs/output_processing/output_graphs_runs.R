#####################################
# OUTPUT PRESENTATION, Forward Runs #
#####################################

# This script is meant to generate graphs which will allow one to visualize the output from the forward runs. 

library(ggplot2)
library(car)

dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")

# Modify dat.all dataframe. 
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.all$SLXSAND <- car::recode(dat.all$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.all$SM_FIRE <- car::recode(dat.all$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

# My theme
mytheme <- theme(plot.title = element_text(hjust = 0.5), # should center the title somewhat
                 panel.background = element_rect(fill="white"), # makes panel background white
                 panel.grid = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(color="black", size = 1.5),
                 axis.text.x = element_text(size = 55, margin = margin(t=20)),
                 axis.text.y = element_text(size = 55, margin = margin(r=20)),
                 axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
                 axis.title.y = element_text(size = 60, face = "bold", margin = margin(t = 50, r = 20)),
                 legend.title = element_text(size=60),
                 legend.text = element_text(size=55, margin = margin(t = 20)),
                 legend.key.size = unit(3, "line"),
                 text = element_text(family = "Helvetica")) # changes font to Helvetica


# ----------------------------------------------------
# Graphs that demonstrate how AGB changes through time
# ----------------------------------------------------

# Prepare table
dat.agb <- dat.all[,1:6] # Subset original datatable to only include agb so that the data is reasonable
dat.agb <- aggregate(dat.agb[,"w.agb"], by=dat.agb[,c("RUNID","year")], FUN=sum, na.rm=T)# Sum agb for each RUNID

# Graph comparing runs
png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/runs_agb.png", width = 2000, height = 1500)
ggplot(dat.agb, aes(x = year, y = x, color = RUNID)) + 
  geom_line() +
  mytheme + 
  xlab("Years since 1800") + 
  ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")"))))
dev.off()


# -------------------------------------------------
# Prepare a dataframe with everything that you want
# -------------------------------------------------

summary(dat.all)
dat.first <- subset(dat.all, subset=dat.all$year <= 24) # Subset so that only the first few years (from 0-24) are taken
dat.first$chunk <- "First 25 Years"
summary(dat.first)
dat.last <- subset(dat.all, subset=dat.all$year >= 190) # Subset so that only the last 25 years (from 190-214) are taken
dat.last$chunk <- "Last 25 Years"
summary(dat.last)

dat.graph <- rbind(dat.first, dat.last)

# ----
# Graph that compares AGB in the first 25 years to AGB in the last 25 years in each scenario
# -----

# Prepare table for agb
dat.graph_agb <- aggregate(dat.graph[,"w.agb"], by=dat.graph[,c("RUNID","year","chunk","SLXSAND","SM_FIRE")], FUN=sum, na.rm=T)# Sum agb for each RUNID

png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/stability_graph.v5.png", width = 2000, height = 1500)
ggplot(dat.graph_agb, aes(x = SM_FIRE, y = x, fill = chunk)) + 
  facet_wrap(~SLXSAND) +
  geom_boxplot(position = position_dodge(width=1), lwd=0.7) + 
  mytheme + 
  xlab("Sand Proportion") + 
  ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")")))) 
dev.off()

# This graph should be a bar chart of the difference between ranges for each run
dat.first <- aggregate(dat.first[,"w.agb"], by=dat.first[,c("RUNID","SLXSAND","SM_FIRE")], FUN=range, na.rm=T)
colnames(dat.first) <- c("RUNID","SLXSAND","SM_FIRE","AGB_First_25_Years")
dat.last <- aggregate(dat.last[,"w.agb"], by=dat.last[,c("RUNID","SLXSAND","SM_FIRE")], FUN=range, na.rm=T)
colnames(dat.last) <- c("RUNID","SLXSAND","SM_FIRE","AGB_Last_25_Years")
dat.graph_range <- merge(dat.first, dat.last)
dat.graph_range$x <- dat.graph_range$AGB_Last_25_Years - dat.graph_range$AGB_First_25_Years

# dat.graph_agb$SM_FIRE <- factor(FRI$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0))

ggplot(dat.graph_range, aes(x=SM_FIRE, y=x, fill=chunk) + 
  geom_bar(position="dodge", stat="identity") + 
    facet_wrap(~SLXSAND) + 
  # geom_text(aes(x=SLXSAND, label=FRI_label, y=FRI+2, group=SM_FIRE), position=position_dodge(1), hjust=0.5, fontface="bold", size=14) +
  # geom_text(data=dat.all.agg[dat.all.agg$SM_FIRE %in% c(0.04, 0.03, 0.02, 0),], aes(x=SLXSAND, label=round(FRI), y=FRI+2, group=SM_FIRE), position=position_dodge(1)) 
  xlab("SM_FIRE") + 
  ylab("Aboveground Biomass (Kg C", " m"^"-2",")"))

###################
# Analysis Script #
###################

dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_runs.v5.csv")

# Modify dat.all dataframe. 
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.all$SLXSAND <- car::recode(dat.all$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.all$SM_FIRE <- car::recode(dat.all$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

summary(dat.all)
dat.first <- subset(dat.all, subset=dat.all$year <= 24) # Subset so that only the first few years (from 0-24) are taken
dat.first$chunk <- "First 25 Years"
summary(dat.first)
dat.last <- subset(dat.all, subset=dat.all$year >= 190) # Subset so that only the last 25 years (from 190-214) are taken
dat.last$chunk <- "Last 25 Years"
summary(dat.last)

dat.analy <- aggregate(dat.first[c("w.agb")], by=dat.first[c("Soil","Fire","RUNID")], FUN=mean)
colnames(dat.analy) <- c("Soil","Fire","RUNID","agb.First_25_Years")
dat.tmp <- aggregate(dat.last[c("w.agb")], by=dat.last[c("Soil","Fire","RUNID")], FUN=mean)
colnames(dat.tmp) <- c("Soil","Fire","RUNID","agb.Last_25_Years")
dat.analy <- merge(dat.analy, dat.tmp)
rm(dat.tmp)
dat.analy$difference <- dat.analy$agb.Last_25_Years - dat.analy$agb.First_25_Years

# Aggregate based on fire
dat.fire <- aggregate(dat.analy[c("difference")], by=dat.analy[c("Fire")], FUN=mean)
dat.sd <- aggregate(dat.analy[c("difference")], by=dat.analy[c("Fire")], FUN=sd)
dat.fire$sd <- dat.sd$difference
rm(dat.sd)
dat.fire$Fire <- car::recode(dat.fire$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")

# Aggregate based on soil
dat.soil <- aggregate(dat.analy[c("difference")], by=dat.analy[c("Soil")], FUN=mean)
dat.sd <- aggregate(dat.analy[c("difference")], by=dat.analy[c("Soil")], FUN=sd)
dat.soil$sd <- dat.sd$difference
rm(dat.sd)

# ------------------------ # 
# Graph for above analysis # 
# ------------------------

ggplot(dat.fire, aes(x=Fire, y=difference)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(aes(ymin=difference-sd, ymax=difference+sd), width=0.2, position=position_dodge(.9)) + 
  # geom_text(data=dat.all.agg[dat.all.agg$SM_FIRE %in% c(0.04, 0.03, 0.02, 0),], aes(x=SLXSAND, label=round(FRI), y=FRI+2, group=SM_FIRE), position=position_dodge(1)) +
  xlab("Fire") + 
  ylab("Difference in agb between first and last 25 years") 

ggplot(dat.soil, aes(x=Soil, y=difference)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(aes(ymin=difference-sd, ymax=difference+sd), width=0.2, position=position_dodge(.9)) +
  # geom_text(data=dat.all.agg[dat.all.agg$SM_FIRE %in% c(0.04, 0.03, 0.02, 0),], aes(x=SLXSAND, label=round(FRI), y=FRI+2, group=SM_FIRE), position=position_dodge(1)) +
  xlab("Soil Type") + 
  ylab("Difference in agb between first and last 25 years") 
