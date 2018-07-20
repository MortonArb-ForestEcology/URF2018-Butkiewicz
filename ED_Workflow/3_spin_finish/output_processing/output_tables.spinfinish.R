###################################################
# Above Ground Biomass (Spinfinish) Output Tables #
###################################################

library(ncdf4)


all.runs <- dir("../extracted_output.v3/") 

for(RUNID in all.runs){
  path.nc <- file.path("../extracted_output.v3",RUNID) # path.nc now contains our file path. 
  files.nc <- dir(path.nc, "ED2") # files.nc can now be used to extract a specific file. ED2 specifies that it pulls the correct
  # data files. 
  # files.nc <- files.nc[1001:length(files.nc)]
  print(RUNID) # This keeps track of whether the function is actually working. 
  for(i in 1:length(files.nc)){
    print(i)
    test.nc <- nc_open(file.path(path.nc,files.nc[i])) # opens a connection to the output file. 
    # days <- test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals #Adds days on to the end of the calendar. 
    days <- ncvar_get(test.nc, "time") #extracts the values for time
    
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
  
} # Close RUNID loop

write.csv(dat.out,paste0("./output_control_runsL.csv"), row.names=F) #This will write the data for each individual run to a CSV file

#The .csv should now be extracted form the server and onto a computer with some sort of graphics software in order to produce the graphs. 
