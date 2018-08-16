######################################
# Create Fractional Basal Area Table #
######################################
# Cori L. Butkiewicz, 2018

# In order to evaluate ecosystem state on the prairie-forest boundary, we need to classify the ecosystem as a prairie, a forest, or a 
# savanna. Most authors define a savanna as between 10 and 50% tree cover, so anything outside of these bounds will be classified as a 
# forest or a prairie. However, because ED2 doesn't give us canopy cover, we're using Basal Area as a proxy.

# For the following code to work, the output_tables.spinfinish code must have been run and the results saved to a .csv. 

# Read in the output: 
my_output <- read.csv("./output_runs_ALL.csv")
summary(my_output)

# We're only interested in the last 90 years of output: 
my_output <- subset(my_output,subset=my_output$year>410)

# Create a new table with the basal areas from my_output: 
table.ba <- aggregate(my_output[,"p.ba"], by=my_output[,c("RUNID","year")], FUN=sum, na.rm=T) # Sums the BA of the entire ecosystem, which
# should give an approximation of the ecosystem's total area. 
colnames(table.ba) <- c("RUNID","year","Total.BA")

# Look at grass BA: 
grass.ba <- subset(my_output,subset=pft=="Grasses")
grass.ba <- grass.ba[,c("RUNID","year","p.ba")]
colnames(grass.ba)[3] <- "Grass.BA"
table.ba <- merge(table.ba, grass.ba, all.x=T)

# Look at tree (diameter >10 cm) BA: 
tree.ba <- subset(my_output, subset=pft=="Hardwoods")
tree.ba <- tree.ba[,c("RUNID","year","p.ba.tree")]
colnames(tree.ba)[3] <- "Tree.BA"
table.ba <- merge(table.ba, tree.ba, all.x=T)

table.ba[is.na(table.ba)] <- 0 # Replace all NA values with 0. 

table.ba$Tree.BA_Fraction <- table.ba$Tree.BA/table.ba$Total.BA # Calculate fractional tree cover.  

# I wrote the following script in order to better group and graph the output. I added a new column that defines each run by the soil 
# moisture threshold, and another column that defines each run by its climate and soil variables. 

# Split each RUNID into its two major components: environment and fire. 
RUNID <- as.character(table.ba$RUNID)
class(RUNID)
factors <- t(data.frame(strsplit(RUNID, split="-F")))
rownames(factors) <- c() # Gets rid of the row names that show up.
colnames(factors) <- c("Environ","Fire") # Generate more user-friendly column names. 
factors <- data.frame(factors)

table.ba <- cbind(factors,table.ba)

# Change the names of the Climate variables. 
levels(table.ba$Environ)[levels(table.ba$Environ)=="CD-SS"] <- "Dry Climate, Low Soil Water Holding Capacity"
levels(table.ba$Environ)[levels(table.ba$Environ)=="CD-SC"] <- "Dry Climate, High Soil Water Holding Capacity"
levels(table.ba$Environ)[levels(table.ba$Environ)=="CW-SS"] <- "Wet Climate, Low Soil Soil Water Holding Capacity"
levels(table.ba$Environ)[levels(table.ba$Environ)=="CW-SC"] <- "Wet Climate, High Soil Water Holding Capacity"

# Change the names of the Fire threshold values. 
levels(table.ba$Fire)[levels(table.ba$Fire)=="N-TN-IN"] <- "No Fire"
levels(table.ba$Fire)[levels(table.ba$Fire)=="Y-TL-IM"] <- "Low Fire Threshold"
levels(table.ba$Fire)[levels(table.ba$Fire)=="Y-TH-IM"] <- "High Fire Threshold"

# The following code is optional. Graphing the data in R organizes my panels in a way that I don't like, so I've controlled the way R 
# makes the graphs by including a "code" that orders the graphs the way I want. 

# Create two dummy columns with a "code" that will order the output graph in a way that I wannt: 
table.ba$Environ.code <- table.ba$Environ
table.ba$Fire.code <- table.ba$Fire

# Recode the names in the dummy column: 
levels(table.ba$Environ.code)[levels(table.ba$Environ.code)=="Dry Climate, Low Soil Water Holding Capacity"] <- "d"
levels(table.ba$Environ.code)[levels(table.ba$Environ.code)=="Dry Climate, High Soil Water Holding Capacity"] <- "b"
levels(table.ba$Environ.code)[levels(table.ba$Environ.code)=="Wet Climate, Low Soil Soil Water Holding Capacity"] <- "c"
levels(table.ba$Environ.code)[levels(table.ba$Environ.code)=="Wet Climate, High Soil Water Holding Capacity"] <- "a"

levels(table.ba$Fire.code)[levels(table.ba$Fire.code)=="No Fire"] <- 1
levels(table.ba$Fire.code)[levels(table.ba$Fire.code)=="Low Fire Threshold"] <- 2
levels(table.ba$Fire.code)[levels(table.ba$Fire.code)=="High Fire Threshold"] <- 3

