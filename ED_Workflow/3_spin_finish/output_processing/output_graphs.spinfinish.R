#######################
# Graphing the Output #
#######################

# This is not meant to be run on the GitHub repository, but instead provides an example of how I generated the graphs in my results.

library(ggplot2)
files.output <- file.path("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/")
FRI <- read.csv("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_FRI_v4.csv")

my_output_general <- read.csv("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_runs_v4.csv")
RUNID <- as.character(my_output_general$RUNID)
factors <- t(data.frame(strsplit(RUNID, split="-f")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Soil","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)
my_output_general <- cbind(factors,my_output_general)
my_output_general$Soil <- car::recode(my_output$Soil, "'s1'='0.93'; 's2'='0.8'; 's3'='0.66'; 's4'='0.52'; 's5'='0.38'")
my_output_general <- merge(my_output_general,FRI)

my_output <- read.csv("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_batables_v4.csv")
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

# Preliminary graphs
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
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off()

# Graphs tree basal area as it changes through time. 
pdf("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_BA_v4")
ggplot(my_output_general,aes(x=year,y=p.ba.tree)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Basal Area") + 
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off()

# Graphs tree stem density as it changes through time. 
pdf("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_density_v4")
ggplot(my_output_general,aes(x=year,y=p.dens.tree)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("Stem Density") + 
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off()

# Graphs maximum DBH as it changes through time. 
pdf("../../../../../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/line_graph_DBH_v4")
ggplot(my_output_general,aes(x=year,y=dbh.max)) +
  geom_line() +
  xlab("Years") +
  facet_wrap( ~ RUNID,ncol=5) + 
  ylab("DBH") + 
  # ggtitle("Fractional Mature Tree Cover\nas a Proxy for Ecosystem State") +
  mytheme
dev.off()

# Generate scatter plot for soil moisture

ggplot(my_output,aes(x=Soil,y=Tree.BA_Fraction)) +
  geom_point() +
  annotate("rect",xmin=0, xmax=6, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=6, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=6, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  xlab("Sand Compposition") +
  ylab("Mature Tree Cover") +
  mytheme

my_output_soil <- aggregate(my_output[,"Tree.BA_Fraction"],by=list(my_output[,c("Soil")]), FUN=mean, na.rm=T)
colnames(my_output_soil) <- c("Soil","Tree.BA_Fraction")

# Generate scatter plot for soil moisture. 
pdf("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/scatterplot_soil_v4.pdf")
ggplot(my_output_soil,aes(x=Soil,y=Tree.BA_Fraction)) +
  geom_point() +
  annotate("rect",xmin=0, xmax=6, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=6, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=6, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  xlab("Sand Composition") +
  ylab("Mature Tree Cover") +
  mytheme
dev.off()

my_output_fire <- aggregate(my_output[,"Tree.BA_Fraction"],by=list(my_output[,c("FRI")]), FUN=mean, na.rm=T)
colnames(my_output_fire) <- c("FRI","Tree.BA_Fraction")

# Generate scatter plot for fire. 
pdf("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/scatterplot_fri_v4.pdf")
ggplot(my_output_fire,aes(x=FRI,y=Tree.BA_Fraction)) + 
  geom_point() +
  annotate("rect",xmin=0, xmax=35, ymin=0.50, ymax=1, alpha=0.4, fill="green") + 
  annotate("rect", xmin=0, xmax=35, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("rect", xmin=0, xmax=35, ymin=0, ymax=0.10, alpha=0.4, fill="yellow") +
  xlab("Fire Return Interval") +
  ylab("Mature Tree Cover") +
  mytheme
dev.off()
