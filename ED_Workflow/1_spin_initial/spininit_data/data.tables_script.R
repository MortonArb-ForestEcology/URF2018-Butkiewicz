####################################
# Above Ground Biomass Output Tables #
####################################

# The following code should be run on the Forest Ecology Server, where the output is.

library(ncdf4)
library(car)

all.runs <- dir("../extracted_output/") #file path

for(RUNID in all.runs){
  path.nc <- file.path("../extracted_output",RUNID) # here's our file path, which should change as the for loop iterates through
  # RUNID. 
  files.nc <- dir(path.nc, "ED2")
  print(RUNID) #Keep track of where the function is currently working. 
  for(i in 1:length(files.nc)){
    print(i) #Keeps track of where the function is currently working. 
    test.nc <- nc_open(file.path(path.nc,files.nc[i]))
    day <- ncvar_get(test.nc, "time")

    table.pft <- data.frame(ncvar_get(test.nc,"Cohort_PFT"))
    
    # Setting up a data frame with our time index, etc
    dat.tmp <- data.frame(RUNID = RUNID,
    					  year=i,
    					  month=rep(1:ncol(table.pft), each=nrow(table.pft)),
    					  day=rep(day, each=nrow(table.pft)))
    					  
    # Add in PFT info
    dat.tmp$pft <- stack(table.pft)[,1]

    # Label things with user-friendly names
    dat.tmp$PFT.name <- car::recode(dat.tmp$pft, "'5'='Grasses'; '10'='Hardwoods'") #Recodes the PFT column so that it's now user
    #-friendly, replacing the old PFT values of 5 and 10. 

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
    
    if(i==1 & RUNID==all.runs[1]){
     dat.out <- dat.tmp2
    } else {
      dat.out <- rbind(dat.out, dat.tmp2)
    }
    nc_close(test.nc)
  } # Close i loop
  # if(RUNID==all.runs[1]){
  #   dat.out_final <- dat.out
  # } else {
  #   dat.out_final <- rbind(dat.out_final,dat.out)
  # }
} # Close RUNID loop

write.csv(dat.out_final,paste0("./tables/output_runs_ALL.csv"), row.names=F) #This will write the output to a .csv
