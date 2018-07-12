####################################
# Above Ground Biomass Data Tables #
####################################

# This code is means to do a couple of things:
#  1) Cycle through each extracted run folder.
#  2) Cycle through each year in that run, building a temporary data frame and then appending the temporary data frame
#     to a large data frame. 
#  3) Save the data frame to a .csv for each run in a new folder under 1_spin_init/spininit_data, called "tables." 
#  4) Except that #3 doesn't work. 
#  5) I kind of know why but I don't know how to fix it. 
#  6) I've never saved anything to a dynamic file path before. 

library(ncdf4)

if(!dir.exists("./tables/")) dir.create("./tables/", recursive = T) #This should create a folder for the output to go into. 

#This is stored in and run from the "spininit_data" folder that I made. 
#I think that this is how I should run this on the server. 
all.runs <- dir("../extracted_output/") 

for(RUNID in all.runs){
  path.nc <- file.path("../extracted_output",RUNID)
  files.nc <- dir(path.nc)
  files.nc <- files.nc[1001:length(files.nc)]
  print(RUNID) #This should help me keep track of where the function is currently working. 
  for(i in 1:length(files.nc)){
    print(i)
    if(i==1){
      test.nc <- nc_open(file.path(path.nc,files.nc[i])) 
      table.pft <- data.frame(ncvar_get(test.nc,"Cohort_PFT"))
      
      agb.trees <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
      agb.trees[table.pft!=10] <- NA #Gets rid of all other plant functional types. 
      agb.trees <- data.frame(agb=colSums(agb.trees,na.rm=TRUE)) #Essentially combines the cohorts, ignoring NA values
      agb.trees$pft <- "Hardwoods"
      
      agb.grasses <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
      agb.grasses[table.pft!=5] <- NA #Gets rid of all other plant functional types. 
      agb.grasses <- data.frame(agb=colSums(agb.grasses,na.rm=TRUE))
      agb.grasses$pft <- "Grasses"
      
      agb.data <- rbind(agb.grasses,agb.trees) #Creates a messy but functional dataframe. 
      
      days <- c(test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals) #Adds days on to the end of the calendar. 
      agb.data$days <- days
      
    } else {
      test.nc <- nc_open(file.path(path.nc,files.nc[i]))
      table.pft <- data.frame(ncvar_get(test.nc,"Cohort_PFT"))
      
      agb.trees <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
      agb.trees[table.pft!=10] <- NA
      agb.trees <- data.frame(agb=colSums(agb.trees,na.rm=TRUE))
      agb.trees$pft <- "Hardwoods"
      
      agb.grasses <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
      agb.grasses[table.pft!=5] <- NA
      agb.grasses <- data.frame(agb=colSums(agb.grasses,na.rm=TRUE))
      agb.grasses$pft <- "Grasses"
      
      agb.data_temp <- rbind(agb.grasses,agb.trees) #Creates a temporary data frame that I can work with. 
      
      days <- c(test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals+days[length(days)]) #Should add days to the end of the 
      # previous vector, giving a continuous stream of days. 
      agb.data_temp$days <- days #Gives us the right number of columns to bind with the pre-made data frame. 
      
      agb.data <- rbind(agb.data,agb.data_temp)
    }
  }
  nc_close(test.nc)
  rownames(agb.data) <- c(1:length(agb.data$agb)) #Makes the rows easier to look at, since they're X1, X63 or
  # something annoying like that. 
  write.csv(agb.data,paste0("./tables/data_",RUNID,".csv")) #This will write the data for each individual run to a CSV file that
  # I can work with in R on my laptop, since the server doesn't have a graphics card. 
}
