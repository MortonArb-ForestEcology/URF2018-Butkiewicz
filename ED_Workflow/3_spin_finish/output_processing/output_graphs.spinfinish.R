#####################
# Spinfinish Graphs # 
#####################

# This script is meant to generate graphs which will allow one to visualize the output from the spinfinish runs. 

# Load appropriate packages into library
library(ggplot2)
library(car)

# ------------------------------------------------------------------------------------
# Prepare dataframes: may need to change file paths depending on where script is run.
# ------------------------------------------------------------------------------------

dat.all <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_spinfinish.v5.csv") # Reads in dataframe with pft, agb, density, dbh, basal area, etc.

# Modify dat.all dataframe. 
RUNID <- as.character(dat.all$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
dat.all <- cbind(factors,dat.all) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
dat.all$SLXSAND <- car::recode(dat.all$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
dat.all$SM_FIRE <- car::recode(dat.all$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
dat.all$FRI <- car::recode(FRI$FRI, "'Inf'='100'")
dat.all$SM_FIRE <- factor(dat.all$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0)) #says SM_FIRE is a factor and telling it what order we should always list things in 

# # Modify FRI table to eliminate ungraphable "Inf" values
# FRI$FRI <- car::recode(FRI$FRI, "'Inf'='100'")
# min_FRI <- min(FRI$FRI)
# max_FRI <- max(FRI$FRI)

# -----------------------
# Set up theme for graphs
# -----------------------

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


# --------
# Graph basal area vs soil texture
# ----------------------------------

# Prepare datatable for trees
dat.tree <- subset(dat.all, subset=dat.all$pft=="Hardwoods")

# Graph results for trees
png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/ba_tree.v5.png", width = 2000, height = 1500)
ggplot(dat.tree, aes(x = SLXSAND, y = w.ba.tree, fill = SM_FIRE)) + 
  geom_boxplot(position = position_dodge(width=1), lwd=0.7) + 
  mytheme + 
  xlab("Sand Proportion") + 
  ylab (expression(bold(paste("Basal Area (cm"^"2", " m"^"-2",")")))) + 
  scale_fill_manual(name = "Fire\nThreshold", values=c("orange", "gold3", "lightgoldenrod2", "olivedrab3", "olivedrab4"))
dev.off()

# Prepare dataframe for grasses
dat.grass <- subset(dat.all, subset=dat.all$pft=="Grasses")

png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/ba_grass.v5.png", width = 2000, height = 1500)
ggplot(dat.grass, aes(x = SLXSAND, y = w.ba, fill = SM_FIRE)) + 
  geom_boxplot(position = position_dodge(width=1), lwd=0.7) + 
  mytheme + 
  xlab("Sand Proportion") + 
  ylab (expression(bold(paste("Basal Area (cm"^"2", " m"^"-2",")")))) + 
  scale_fill_manual(name = "Fire\nThreshold", values=c("orange", "gold3", "lightgoldenrod2", "olivedrab3", "olivedrab4"))
dev.off()

# ----
# Graph density vs soil texture
# --------------------------------

ggplot(dat.all, aes(x=SLXSAND, y=p.dens.tree, fill=SM_FIRE)) + 
  geom_boxplot() + 
  scale_fill_manual(name = "Fire\nThreshold", values = c("orange", "gold3", "lightgoldenrod3", "olivedrab3", "olivedrab4"))

# ----
# Plot Fire return interval vs. SM_FIRE
# -----------------

# Format datatable
FRI <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_tables/output_FRI.spinfinish.v5.csv") # Reads in dataframe with a fire return interval (in years) for each runID.
RUNID <- as.character(FRI$RUNID) # Stores RUNID as a character
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
FRI <- cbind(factors,FRI) # Allows me to sort data based on soil or fire, instead of soil and fire combinations. 
FRI$SLXSAND <- car::recode(FRI$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
FRI$SM_FIRE <- car::recode(FRI$Fire, "'1'='0.04'; '2'='0.03'; '3'='0.02'; '4'='0.01'; '5'='0'")
FRI$FRI <- car::recode(FRI$FRI, "'Inf'='100'")
FRI$SM_FIRE <- factor(FRI$SM_FIRE, levels=c(0.04, 0.03, 0.02, 0.01, 0))

FRI$FRI_label <- round(FRI$FRI) # Creates new column with rounded FRI values so that FRI of 1.6 becomes FRI of 2
# FRI[FRI$FRI==100 & dat.all$SM_FIRE!= "0.01","FRI_label"] <- NA 
summary(FRI) # Check formatting

# Plot graph
png("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/v5_graphs/FRI.v5.png", width = 2000, height = 1500)
ggplot(FRI, aes(x=SLXSAND, y=FRI, fill=SM_FIRE, label=FRI)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(x=SLXSAND, label=FRI_label, y=FRI+2, group=SM_FIRE), position=position_dodge(1), hjust=0.5, fontface="bold", size=14) +
  # geom_text(data=dat.all.agg[dat.all.agg$SM_FIRE %in% c(0.04, 0.03, 0.02, 0),], aes(x=SLXSAND, label=round(FRI), y=FRI+2, group=SM_FIRE), position=position_dodge(1)) +
  xlab("Sand Proportion") + 
  ylab("Fire Return Interval (years)") + 
  scale_fill_manual(name = "Fire\nThreshold", values=c("orange", "gold3", "lightgoldenrod2", "olivedrab3", "olivedrab4")) + 
  guides(color=F) +
  mytheme
dev.off()
