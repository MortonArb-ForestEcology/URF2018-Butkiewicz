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
  files.nc <- dir(path.nc, "ED2")
  # files.nc <- files.nc[1001:length(files.nc)]
  print(RUNID) #This should help me keep track of where the function is currently working. 
  for(i in 1:length(files.nc)){
    print(i)
    test.nc <- nc_open(file.path(path.nc,files.nc[i])) 
    # days <- test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals #Adds days on to the end of the calendar. 
    days <- ncvar_get(test.nc, "time")

    table.pft <- data.frame(ncvar_get(test.nc,"Cohort_PFT"))
    
    # Setting up a data frame with our time index, etc
    dat.tmp <- data.frame(RUNID = RUNID,
    					  year=i,
    					  month=rep(1:ncol(table.pft), each=nrow(table.pft)),
    					  day=rep(days, each=nrow(table.pft)))
    					  
    # Add in PFT info
    dat.tmp$pft <- stack(table.pft)[,1]

    # Label things with user-friendly names
    dat.tmp$PFT.name <- car::recode(dat.tmp$pft, "'5'='Grasses'; '10'='Hardwoods'")

    # Add in AGB  
    agb.trees <- data.frame(ncvar_get(test.nc,"Cohort_AbvGrndBiom"))
    dat.tmp$AGB <- stack(agb.trees)[,1]
    
    density.trees <- data.frame(ncvar_get(test.nc,"Cohort_Density"))
    dat.tmp$density <- stack(density.trees)[,1]

    dbh.trees <- data.frame(ncvar_get(test.nc,"Cohort_DBH"))
    dat.tmp$DBH <- stack(dbh.trees)[,1]
    
    # Condensing to 1 point per PFT per time
    dat.tmp2 <- aggregate(dat.tmp[,c("AGB", "density")], by=dat.tmp[,c("RUNID", "year", "month", "day", "PFT.name")], FUN=sum)
    # names(dat.tmp2)[names(dat.tmp2)=="x"] <- "AGB" # When workign with 2+ vars, it preserves names
        
    dat.tmp2$DBH.mean <- aggregate(dat.tmp[,c("DBH")], by=dat.tmp[,c("RUNID", "year", "month", "day", "PFT.name")], FUN=mean)[,"x"]
    dat.tmp2$DBH.sd <- aggregate(dat.tmp[,c("DBH")], by=dat.tmp[,c("RUNID", "year", "month", "day", "PFT.name")], FUN=sd)[,"x"]
    dat.tmp2$DBH.min <- aggregate(dat.tmp[,c("DBH")], by=dat.tmp[,c("RUNID", "year", "month", "day", "PFT.name")], FUN=min)[,"x"]
    dat.tmp2$DBH.max <- aggregate(dat.tmp[,c("DBH")], by=dat.tmp[,c("RUNID", "year", "month", "day", "PFT.name")], FUN=max)[,"x"]
    
    if(i==1){
     dat.out <- dat.tmp2
    } else {
      dat.out <- rbind(dat.out, dat.tmp2)
    }
    nc_close(test.nc)
  } # Close i loop
  # I can work with in R on my laptop, since the server doesn't have a graphics card. 
} # Close RUNID loop

write.csv(dat.out,paste0("./tables/output_runs_ALL.csv"), row.names=F) #This will write the data for each individual run to a CSV file that
