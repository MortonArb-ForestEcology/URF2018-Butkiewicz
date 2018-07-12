###############################
# Above Ground Biomass Graphs #
###############################

library(ncdf4)
library(modeest)

#The goal of this code is to produce a graph (or two) that compares the AGB of different plant functional types. 
#First we'll be working with one file, then we'll be working with multiple, until finally we can produce something with the entire server.

# -------------------------------------------- #
# Organizing the extracted output into a table #
# -------------------------------------------- #

#The goals for the following script are to get a table of AGB vs time. For now we can just graph the result in R,but since the server doesn't have a graphics card it would probably be ideal to write the data into a csv, then transfer the csv to my laptop and use another R script to produce the graphs. The csv will be large, but nowhere near as large as the raw data output, so it should all be good.  

path.nc <- "/Users/Cori/Research/Morton_Arb/Project/Die/"
files.nc <- dir(path.nc) #This will have to change, since I'll need to exclude the nc.var files somehow. I have some ideas, but I don't know if it'll work. But for now, this will suffice. 

test.nc <- nc_open(file.path(path.nc,files.nc[1]))
Table.agb <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
Table.pft <- data.frame(ncvar_get(test.nc,"Cohort_PFT")) #5 means it's a C3 grass, 10 means it's a mid-successional hardwood (an oak). 0 means there's no biomass at the site and the cohort does not exist, either yet or anymore. 
Table.pft_name <- data.frame(ncvar_get(test.nc,"PFT_name"))
nc_close(test.nc)

#Okay. So I have a table with the aboveground biomass, and a table with the cohort PFTs. So I need to find some way to combine the cohorts a bit--that means I'm going to have to combine some rows--keep the columns separate. I need to find a way to make sure that each cohort and each month remains associated with each other. 

Table.pft[Table.pft==0] <- NA #Replaces the 0's with NA to make the data easier to manage. 

for(i in 1:nrow(Table.pft)){
  if(i==1){
    temp_vector <- as.numeric(Table.pft[i,])
    print(class(temp_vector))
    x <- mfv(temp_vector)
    modes <- x
    print(modes)
  } else {
    temp_df <- subset(Table.pft[i,])
    temp_vector <- as.numeric(Table.pft[i,])
    x <- mfv(temp_vector)
    modes <- append(modes,values=x)
    print(modes)
  }
}
Table.pft$modes <- modes #This dataframe should now have the modes at the end of the rows.

agb.trees <- Table.agb[Table.pft$modes==10, ]
agb.trees <-data.frame(agb=colSums(agb.trees))
agb.trees$pft <- "Hardwoods"
agb.grasses <- Table.agb[Table.pft$modes==5, ]
agb.grasses <- data.frame(agb=colSums(agb.grasses))
agb.grasses$pft <- "Grasses"

agb.data <- rbind(agb.grasses,agb.trees)

days <- c(test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals)
agb.data$days <- days

#Cleanup
rm(agb.trees,agb.grasses,temp_df,temp_vector,modes,days)

# -------------------------- #
# Graphing the extracted AGB #
# -------------------------- #

library(ggplot2)

plot <- ggplot(agb.data,aes(x=days,y=agb,color=pft))+geom_line()+xlab("Days since 01-01-1801")+ylab("Above Ground Biomass, kg C m-2")
plot + scale_color_manual(values=c("#E69F00","#009E73"))

#This will probably be moved to a separate code so that I can use one on the server and one on my laptop. It'll be fun. I hope. But for now it'll be here so that I can see it.  

