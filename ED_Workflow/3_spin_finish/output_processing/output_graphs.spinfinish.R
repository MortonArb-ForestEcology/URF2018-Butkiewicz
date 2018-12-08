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
min_FRI <- min(FRI$FRI)
max_FRI <- max(FRI$FRI)

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
dat.all.agg <- aggregate(x=dat.all[,c("p.dens","p.dens.tree","p.ba","p.ba.tree","soil_moist","p.agb")],
                         by=dat.all[,c("RUNID","SLXSAND","SM_FIRE")],
                         FUN=mean)
colnames(dat.all.agg)
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

# Standardized Soil Moisture vs. FRI
# pdf("/Users/Cori/Desktop/graph")
ggplot(sm.standard, aes(x=FRI, y=soilmoist)) #Compares soil moisture against fire return interval to see if there's any covariation.
#dev.off()
  
# --------
# OTHER GRAPHS
# ---------

# Theme for the graphs
mytheme <- theme(plot.title = element_text(hjust = 0.5), # should center the title somewhat
                 panel.background = element_rect(fill="white"), # makes panel background white
                 panel.grid = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(color="black", size = 1.5),
                 text = element_text(family = "Helvetica"), # changes font to Helvetica
                 axis.text.x = element_text(size = 55, margin = margin(t=20)),
                 axis.text.y = element_text(size = 55, margin = margin(r=20)),
                 axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
                 axis.title.y = element_text(size = 60, face = "bold", margin = margin(t = 50, r = 20)),
                 legend.title = element_text(size=60),
                 legend.text = element_text(size=55, margin = margin(t = 20)),
                 legend.key.size = unit(3, "line"))

  facet_grid(. ~ SLXSAND) #Labels on the top represent proportion of sand in the soil. 
# dev.off()

dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

summary(dat.all)

# Plots basal area vs. soil texture
png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/basal_area.png", width = 2000, height = 2000)
ggplot(dat.all, aes(x = SLXSAND, y = p.ba.tree, fill = SM_FIRE)) + 
  geom_boxplot(position = position_dodge(width=1), lwd=0.7) + 
  mytheme + 
  xlab("Sand Proportion") + 
  ylab (expression(bold(paste("Basal Area (cm"^"2", " m"^"-2",")")))) + 
  scale_fill_manual(name = "Fire\nThreshold", values=c("orange", "gold3", "lightgoldenrod2", "olivedrab3", "olivedrab4"))
dev.off()

# Plots density vs. soil texture
ggplot(dat.all, aes(x=SLXSAND, y=p.dens.tree, fill=SM_FIRE)) + 
  geom_boxplot() + 
  scale_fill_manual(name = "Fire\nThreshold", values = c("orange", "gold3", "lightgoldenrod3", "olivedrab3", "olivedrab4"))

#Plots FRI vs SM_FIRE
dat.all.agg$SM_FIRE <- factor(dat.all.agg$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

# geom_text(data = censusData,
#           aes(x = County, group=variable, y = value + 150, 
#               label = format(value, nsmall = 0, digits=1, scientific = FALSE)), 
#           color="blue", position=position_dodge(.9), hjust=.5)
dat.all.agg$FRI_label <- round(dat.all.agg$FRI)
dat.all.agg[dat.all.agg$FRI==100 & dat.all.agg$SM_FIRE!= "0.01","FRI_label"] <- NA
summary(dat.all.agg)
png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/fire.png", width = 2000, height = 1500)
ggplot(dat.all.agg, aes(x=SLXSAND, y=FRI, fill=SM_FIRE, label=FRI)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(x=SLXSAND, label=FRI_label, y=FRI+2, group=SM_FIRE), position=position_dodge(1), hjust=0.5, fontface="bold", size=14) +
  # geom_text(data=dat.all.agg[dat.all.agg$SM_FIRE %in% c(0.04, 0.03, 0.02, 0),], aes(x=SLXSAND, label=round(FRI), y=FRI+2, group=SM_FIRE), position=position_dodge(1)) +
  xlab("Sand Proportion") + 
  ylab("Fire Return Interval (years)") + 
  scale_fill_manual(name = "Fire\nThreshold", values=c("orange", "gold3", "lightgoldenrod2", "olivedrab3", "olivedrab4")) + 
  guides(color=F) +
  mytheme
dev.off()
