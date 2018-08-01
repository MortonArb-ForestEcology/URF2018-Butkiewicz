###################################################
# Above Ground Biomass (Spinfinish) Output Tables #
###################################################

library(ncdf4)

all.runs <- dir("../extracted_output.v3/") 

for(RUNID in all.runs){
  path.nc <- file.path("../extracted_output.v3",RUNID)
  files.nc <- dir(path.nc, "ED2")
  print(RUNID) #Keep track of where the function is currently working. 
  # for(i in 1:length(files.nc)){
  for(i in 1:length(files.nc)){
    print(i) #Keeps track of where the function is currently working. 
    ncT <- nc_open(file.path(path.nc,files.nc[i]))
    
    table.patch <- as.data.frame(matrix(ncvar_get(ncT,"Cohort_PatchID"),ncol=12)) # This seems unnecessarily complex, but this makes sure that even if there is only one cohort, the dataframes will be shaped the same way. 
    table.pft <- as.data.frame(matrix(ncvar_get(ncT, "Cohort_PFT"),ncol=12))
    table.agb <- as.data.frame(matrix(ncvar_get(ncT, "Cohort_AbvGrndBiom"),ncol=12))
    table.dens <- as.data.frame(matrix(ncvar_get(ncT, "Cohort_Density"),ncol=12))
    table.dbh <- as.data.frame(matrix(ncvar_get(ncT, "Cohort_DBH"),ncol=12))
    
    dat.cohort <- data.frame(month=rep(1:ncol(table.pft),each=nrow(table.pft)), #The choice of table.pft to count the number of months is arbitrary. 
                             patch = stack(table.patch)[,1],
                             pft = stack(table.pft)[,1], # kgC/m2
                             agb = stack(table.agb)[,1],
                             dens = stack(table.dens)[,1], # trees/m2
                             dbh = stack(table.dbh)[,1]) # DBH/tree
    
    # Calculate the DBH weight for each cohort usign a loop!
    dat.cohort$p.dens <- NA # creating a placeholder column
    dat.cohort$p.dbh <- NA # creating a placeholder column
    dat.cohort$dbh.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dbh, NA) #This creates a column of DBH that only includes values from the dbh column that are greater than 10 cm. 
    
    # Basically, "variable" is the measurement of every single tree in the patch. 
    #            "variable.tree" is the measurement of every tree with a dbh >10 cm. 
    #            "p.variable" is the measurement weighted by patch area, which in ED is outputted as a proportion and has 
    #             no units. 
    
    dat.cohort$dens.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dens, NA) #This creates a new column where only the densities associated with a DBH > 10 cm is allowed. 
    dat.cohort$p.dbh.tree <- NA #creates a placeholder column, which will be the tree DBH weighted by patch area. 
    dat.cohort$p.dens.tree <- NA #creates a placeholder column, which will be the tree density weighted by patch area. 
    
    #Make a loop that calculates the weighted values for dbh.  
    for(PCH in unique(dat.cohort$patch)){
      for(PFT in unique(dat.cohort$pft)){
        row.ind <- which(dat.cohort$patch==PCH & dat.cohort$pft==PFT) # row numbers for this group 
        
        dat.tmp <- dat.cohort[row.ind,] # subset our data to something small for our sanity
        dens.tot <- sum(dat.tmp$dens) # sum of cohort densities by patch and PFT. 
        dens.tree <- sum(dat.tmp$dens.tree, na.rm=T) # total density of the trees with a DBH above our threshold value. 
        
        dat.tmp$p.dens <- dat.tmp$dens/dens.tot # fractional density. 
        dat.tmp$p.dens.tree  <- dat.tmp$dens.tree/dens.tree # density of trees above our DBH weighted by patch area
        dat.tmp$p.dbh  <- dat.tmp$dbh * dat.tmp$p.dens 
        dat.tmp$p.dbh.tree  <- dat.tmp$dbh.tree * dat.tmp$p.dens.tree
        
        dat.cohort[row.ind,c("p.dens", "p.dbh", "p.dens.tree", "p.dbh.tree")] <- dat.tmp[,c("p.dens", "p.dbh", "p.dens.tree", "p.dbh.tree")] # put the new values into our table
      } # Close PFT loop
    } # Close PCH (patch) loop 
    
    dat.patch <- aggregate(dat.cohort[,c("agb", "dens", "p.dbh", "dens.tree", "p.dbh.tree")], by=dat.cohort[,c("patch", "pft")], FUN=sum, na.rm=T)
    dat.patch$dbh.max <- round(aggregate(dat.cohort$dbh, by=dat.cohort[,c("patch", "pft")], FUN=max)[,"x"],2) # rounding to 2 decimal places
    names(dat.patch) <- car::recode(names(dat.patch), "'p.dbh'='dbh'; 'p.dbh.tree'='dbh.tree'")
    
    patch.area <- ncvar_get(ncT, "Patch_Area")[,6]
    patch.area <- data.frame(patch = 1:length(patch.area),
                             area  = patch.area)
    
    dat.patch <- merge(dat.patch, patch.area, all.x=T)
    
    dat.patch[dat.patch$dens.tree==0, "dbh.tree"] <- NA
    dat.patch$p.agb <- dat.patch$agb * dat.patch$area
    dat.patch$p.dens <- dat.patch$dens * dat.patch$area
    dat.patch$p.dbh <- dat.patch$dbh * dat.patch$area
    dat.patch$p.dens.tree <- dat.patch$dens.tree * dat.patch$area
    
    # For trees, we need to weight by area of patches with TREES
    area.tree <- dat.patch[dat.patch$pft==10 & !is.na(dat.patch$dbh.tree),"area"]
    dat.patch$p.dbh.tree <- dat.patch$dbh.tree * dat.patch$area/sum(area.tree)
    
    dat.site <- aggregate(dat.patch[,c("p.agb", "p.dens", "p.dbh", "p.dens.tree", "p.dbh.tree")], by=list(dat.patch$pft), FUN=sum, na.rm=T)
    dat.site$dbh.max <- aggregate(dat.patch[,"dbh.max"], by=list(dat.patch$pft), FUN=max)[,"x"]
    colnames(dat.site)[1] <- "pft"
    dat.site <- subset(dat.site,subset=pft!=0)
    
    dat.site <- data.frame(RUNID=RUNID,
                           year=i,
                           dat.site)
    
    # Add a binary fire variable. 
    fire <- matrix(ncvar_get(test.nc,"Fire_flux"))
    if(sum(fire)!=0){
      fire <- "Yes" #Means that fire occured. 
    } else {
      fire <- "No" #Means that fire did not occur. 
    }
    dat.site$fire <- fire
    
    if(i==1 & RUNID==all.runs[1]){
      dat.out <- dat.site
    } else {
      dat.out <- rbind(dat.out, dat.site)
    }
    nc_close(ncT)
  } # Close i loop
} # Close RUNID loop

write.csv(dat.out,paste0("./output_runs_ALL.csv"), row.names=F) #This will write the output to a .csv
#The final result should be yearly averages of everything. It would probably be better to weight this by monthly averages, but...well, we're not there yet.
