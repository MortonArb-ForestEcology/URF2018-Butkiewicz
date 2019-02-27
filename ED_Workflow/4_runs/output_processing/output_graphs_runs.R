#############################
# Forward Runs Graph Script #
#############################

# This script is meant to generate graphs which will allow one to visualize the output from the forward runs. 

library(ggplot2)
library(car)

dat.all <- read.csv("./output_runs.v5.csv")

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
# Subset original datatable to only include agb so that the data is reasonable
# Sum agb for each RUNID

# Graph comparing runs
ggplot(dat.agb, aes(x = year, y = agb, color = RUNID)) + 
  mytheme + 
  xlab("Years since 1800") + 
  ylab (expression(bold(paste("Aboveground Biomass (Kg C", " m"^"-2",")"))))

# ----
# Graph that compares BA grasses to BA trees in each scenario
# -----

# Compares based on fire frequency
ggplot(dat.all, aes(x = year, y = ba, color = SM_FIRE)) + 
  facet_grid(.~SLXSAND) +
  mytheme + 
  xlab("Years since 1800") + 
  ylab (expression(bold(paste("Basal Area (cm"^"2", " m"^"-2",")"))))3
  
# Compares based on soil
ggplot(dat.all, aes(x = year, y = ba, color = SLXSAND)) + 
  facet_grid(.~SM_FIRE) +
  mytheme + 
  xlab("Years since 1800") + 
  ylab (expression(bold(paste("Basal Area (cm"^"2", " m"^"-2",")"))))
