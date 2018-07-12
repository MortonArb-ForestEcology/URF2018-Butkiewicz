#############################
# Above Ground Biomass Data #
#############################

# ----------------------- #
# Formatting a Data Table #
# ----------------------- #

library(ncdf4)

if(!dir.exists("./tables/")) dir.create("./tables/", recursive = T) #This should create a folder for the output to go into. 

# Some example code from an extract workflow that has a run ID #

# all.runs <- dir("4_runs/ed_runs.v1")
# for(RUNID in all.runs){
#   
#   # -------
#   # Set up dynamic file paths based on each site we're looping through
#   # -------
#   ed.dir <- file.path("4_runs/ed_runs.v1", RUNID, "analy") # Where the raw data are
#   outdir <- file.path("4_runs/extracted_output", RUNID) # Where we want to save our output
#   if(!dir.exists(outdir)) dir.create(outdir, recursive = T)

###############################################################################################################

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
  write.csv(agb.data,file="./tables/data_",RUNID) #This will write the data for each individual run to a CSV file that
  # I can work with in R on my laptop, since the server doesn't have a graphics card. 
}
