#######################
# Graphing the Output #
#######################

# This is not meant to be run on the GitHub repository, but instead provides an example of how I generated the graphs in my results.

library(ggplot2)
files.output <- file.path("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/")
FRI <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_FRI_v4.csv")

my_output_general <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_runs_v4.csv")
RUNID <- as.character(my_output_general$RUNID)
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
my_output_general <- cbind(factors,my_output_general)
my_output_general$Soil <- car::recode(my_output$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
my_output_general <- merge(my_output_general,FRI)

my_output <- read.csv("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_batables_v4.csv")
my_output <- merge(my_output,FRI)
my_output$Soil <- car::recode(my_output$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")

# my_output_subset <- subset(my_output, subset=year>=410) #Subset last ninety years so that we can look at the stability of the ecosystems.
summary(my_output)

# Gives a theme for this particular graph. 
mytheme <- theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(),
        text=element_text(family="Helvetica"))
# ---------------------------------------------------

# GRAPH OF CHANGE IN FRACTIONAL TREE COVER OVER TIME
# ---------------------------------------------------
# Basal area was calculated from dbh, tree density, and patch area, and then tracked over time. The graph is labeled with green, 
# orange, and yellow regions. If the line falls within the green region, the spinup is a forest. If it falls within the yellow 
# region, it is a prairie, and if it falls within the orange region then it is a savanna. 
pdf("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_treecover_v4.pdf")
ggplot(my_output,aes(x=year,y=Tree.BA_Fraction)) +
  annotate("rect",xmin=0, xmax=101, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=101, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=101, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  annotate("text", x=20, y=0.55, label="Forest", size=2, alpha=0.9, family="Helvetica") + 
  annotate("text", x=25,y=0.15,label="Savanna",size=2,alpha=0.9,family="Helvetica") +
  annotate("text", x=20, y=0.05, label="Prairie", size=2, alpha=0.9, family="Helvetica") +
  # geom_hline(yintercept=0.10,color="#E69F00",size=0.5)+
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Fractional Mature Tree Cover") + 
  ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off() # ----

# GRAPH OF TREE BASAL AREA AS IT CHANGES THROUGH TIME
# ---------------------------------------------------
# Basal area was calculated from dbh, tree density, and patch area (I belive). This is measured in cm^2.  
ggplot(my_output_general,aes(x=year,y=p.ba.tree)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Basal Area, cm^2") + 
  ggtitle("Basal Area Through Time") +
  mytheme
dev.off() # ------

# GRAPH OF THE TREE DENSITY AS IT CHANGES THROUGH TIME
# ----------------------------------------------------
# Tree density was given in the output, measured in plants m^-2. This graph shows density as it changes through time from the year
# 2200 until the year 2300. 
pdf("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_density_v4")
ggplot(my_output_general,aes(x=year,y=p.dens.tree)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Stem Density, plants m^-2") + 
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off() # -----

# GRAPH OF THE MAXIMUM DBH AS IT CHANGES THROUGH TIME
# ----------------------------------------------------
# The max DBH for each patch in each year was determined when the output was extracted into the tables. The maximum DBH is in cm and 
# generally increased through time. 
pdf("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_DBH_v4")
ggplot(my_output_general,aes(x=year,y=dbh.max)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Maximum DBH, cm") + 
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off() # ------

# SCATTER PLOT RELATING MATURE TREE COVER TO SOIL MOISTURE
# ---------------------------------------------------------
# Mature tree cover will vary with soil moisture. The tricky part is that fire frequency also varies with soil moisture.Here I took 
# the fractinal tree cover and  compared it to the soil composition (specifically the sand composition). This can be seen with all of 
# the variation, or it can be seen in the averages. To get a graph of the averages, simply uncomment the first ggplot line and comment
# out the second ggplot line. 

# Data table with average soil values # ------
output_var <- colnames(my_output_general)
output_var <- output_var[6:13]
for (VAR in output_var) {
  my_output_temp <- aggregate(my_output_general[,VAR],by=my_output_general[,c("pft","Soil")],FUN=mean, na.rm=T)
  colnames(my_output_temp) <- c("pft","Soil",VAR)
  if(VAR==output_var[1]){
    my_output_soil <- my_output_temp
  } else {
    my_output_soil <- merge(my_output_soil,my_output_temp)
  } #end if loop
}
my_output_temp <- aggregate(my_output[,"Tree.BA_Fraction"],by=list(my_output[,c("Soil")]), FUN=mean, na.rm=T)
colnames(my_output_temp) <- c("Soil","Tree.BA_Fraction")
my_output_soil <- merge(my_output_soil, my_output_temp)
rm(my_output_temp)

pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/scatterplot_soil_v4.pdf")
ggplot(my_output_soil,aes(x=Soil,y=Tree.BA_Fraction)) +
# ggplot(my_output,aes(x=Soil,y=Tree.BA_Fraction)) +
  geom_point() +
  annotate("rect",xmin=0, xmax=6, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=6, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=6, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  xlab("Sand Compposition") +
  ylab("Mature Tree Cover") +
  mytheme
dev.off # ------

# SCATTER PLOT RELATING MAXIMUM DBH TO SOIL
# ------------------------------------------
pdf("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/DBH&soil_scatterplot.pdf")
ggplot(my_output_soil,aes(x=Soil,y=dbh.max)) +
  # ggplot(my_output,aes(x=Soil,y=dbh.max)) +
  geom_point() +
  xlab("Sand Compposition") +
  ylab("DBH, cm") +
  mytheme
dev.off # ------

# SCATTER PLOT RELATING MATURE TREE COVER TO FIRE
# -----------------------------------------------
# We can also relate fire to the mature tree cover. We can get an idea of what the correlation is, however it should be noted that 
# soil serves as a confounding variable, since the fire return interval tends to increase with increasing soil moisture. We can graph
# this in two ways: the first takes out variation in the dataset by only reporting the average tree cover values for each FRI. The 
# second way graphs all of the variation. There is always one point at the very edge of the graph; this represents the FRI denoted as 
# "Inf", which means that fire was never triggered. 

# Create data table of average values according to FRI #--------
output_var <- colnames(my_output_general)
output_var <- output_var[6:13]
for (VAR in output_var) {
  my_output_temp <- aggregate(my_output_general[,VAR],by=my_output_general[,c("pft","FRI")],FUN=mean, na.rm=T)
  colnames(my_output_temp) <- c("pft","FRI",VAR)
  if(VAR==output_var[1]){
    my_output_fire <- my_output_temp
  } else {
    my_output_fire <- merge(my_output_fire,my_output_temp)
  } #end if loop
}
my_output_temp <- aggregate(my_output[,"Tree.BA_Fraction"],by=list(my_output[,c("FRI")]), FUN=mean, na.rm=T)
colnames(my_output_temp) <- c("FRI","Tree.BA_Fraction")
my_output_fire <- merge(my_output_fire, my_output_temp)
rm(my_output_temp)

# SCATTER PLOT RELATING MATURE TREE COVER TO FIRE
#--------------------------------------------------------
pdf("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/scatterplot_fri_v4.pdf")
ggplot(my_output_fire,aes(x=FRI,y=Tree.BA_Fraction)) + 
# ggplot(my_output, aes(x=FRI,y=Tree.BA_Fraction)) + 
  geom_point() +
  annotate("rect",xmin=0, xmax=35, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=35, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=35, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  xlab("Fire Return Interval") +
  ylab("Mature Tree Cover") +
  mytheme
dev.off() #--------

# SCATTER PLOT RELATING MAXIMUM DBH TO FIRE
# -------------------------------------------
pdf ("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/DBH&FRI_scatterplot")
ggplot(my_output_general, aes(x=FRI,y=dbh.max)) + 
  geom_point() +
  xlab("Fire Return Interval") +
  ylab("Maximum DBH, cm") +
  mytheme
dev.off() #--------

# SCATTER PLOT RELATING TREE DENSITY TO FIRE
# -------------------------------------------
pdf ("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/Density&FRI_scatterplot")
# ggplot(my_output_general, aes(x=FRI,y=p.dens)) + 
ggplot(my_output_fire, aes(x=FRI,y=p.dens)) + 
  geom_point() +
  xlab("Fire Return Interval") +
  ylab("Tree Density, plans m^-2") +
  mytheme
dev.off() #--------

# SCATTER PLOT RELATING TREE DENSITY TO FIRE
# -------------------------------------------
pdf ("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/Density&FRI_scatterplot")
# ggplot(my_output_general, aes(x=FRI,y=p.dens)) + 
ggplot(my_output_fire, aes(x=FRI,y=p.dens)) + 
  geom_point() +
  xlab("Fire Return Interval") +
  ylab("Tree Density, plans m^-2") +
  mytheme
dev.off() #--------

# SCATTER PLOT RELATING BASAL AREA TO FIRE
# -------------------------------------------
pdf ("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/BA&FRI_scatterplot")
# ggplot(my_output_general, aes(x=FRI,y=p.ba)) + 
ggplot(my_output_fire, aes(x=FRI,y=p.ba)) + 
  geom_point() +
  xlab("Fire Return Interval") +
  ylab("Tree Density, plans m^-2") +
  mytheme
dev.off() #--------

