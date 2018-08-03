#######################
# Graphing the Output #
#######################

library(ggplot2)

my_output <- read.csv("./output_runs_ALL.csv")
my_output_subset <- subset(my_output, subset=year>=410) #Subset last ninety years so that we can look at the stability of the ecosystems.
summary(my_output)

# Gives a theme for this particular graph. 
mytheme <- theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(),
        text=element_text(family="Times"))
# ---------------------------------------------------

# GRAPH TREE COVER
# Using Basal Area as a proxy for canopy cover, 
# ------------------------

# Make the Basal Area data table: 
table.ba <- aggregate(my_output_subset[,"p.ba"], by=my_output_subset[,c("RUNID","year")], FUN=sum, na.rm=T)
colnames(table.ba) <- c("RUNID","year","Total.BA")

grass.ba <- subset(my_output_subset,subset=pft=="Grasses")
grass.ba <- grass.ba[,c("RUNID","year","p.ba")]
colnames(grass.ba)[3] <- "Grass.BA"
table.ba <- merge(table.ba, grass.ba, all.x=T)

tree.ba <- subset(my_output_subset, subset=pft=="Hardwoods")
tree.ba <- tree.ba[,c("RUNID","year","p.ba.tree")]
colnames(tree.ba)[3] <- "Tree.BA"
table.ba <- merge(table.ba, tree.ba, all.x=T)

table.ba[is.na(table.ba)] <- 0 # Replace all NA values with 0. 

table.ba$Tree.BA_Fraction <- table.ba$Tree.BA/table.ba$Total.BA

# Make the basal area graph: 

pdf("./output_graph_ECOSYSTEM_TYPE.500.pdf")
ggplot(table.ba,aes(x=year,y=Tree.BA_Fraction)) +
  # geom_vline(data=my_output[my_output_subset$fire=="Yes",], aes(xintercept=year), col="gray", size=0.25) +
  # geom_hline(yintercept=0.50,color="#099E73",size=0.5)+
  annotate("rect", xmin=410, xmax=500, ymin=0.10, ymax=0.50, alpha=0.4, fill="orange") + 
  annotate("text", x=488,y=0.20,label="Savanna",size=3,alpha=0.9,family="Times")+
  # geom_hline(yintercept=0.10,color="#E69F00",size=0.5)+
  geom_line() +
  facet_wrap( ~ RUNID,ncol=3) +
  xlab("Number of Years") +
  ylab("Fractional Mature Tree Cover, kg C m-2") + 
  ggtitle("Fractional Tree Cover\nat 95.45.54127˚N, -95.5313˚E") +
  mytheme
dev.off()
