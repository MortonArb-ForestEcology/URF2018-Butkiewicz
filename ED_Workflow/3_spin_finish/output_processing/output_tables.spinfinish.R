###########################
# Spinfinish Output Table #
###########################

library(ncdf4)
library(car)

all.runs <- dir("../extracted_output.v4/")

for(RUNID in all.runs){
  path.nc <- file.path("../extracted_output.v4",RUNID) #Set up path files.
  files.nc <- dir(path.nc,"ED2") #Lists the files in the RUNID folder; there is one file for each year. 
  
  print(RUNID) #Keep track of where the function is currently working.
  
  for(i in 1:length(files.nc)){ #looking at one year at a time
    print(i) #Keep track of where function is currently working. Each i represents a year being opened 
    ncT <- nc_open(file.path(path.nc,files.nc[i]))
    
    dat.cohort <- data.frame(RUNID=RUNID,
                             year=i,
                             patch = ncvar_get(ncT, "Cohort_PatchID")[,7], #Just extract row 7, which represents the month of July
                             pft   = ncvar_get(ncT, "Cohort_PFT")[,7], 
                             agb   = ncvar_get(ncT, "Cohort_AbvGrndBiom")[,7], # kgC/m2
                             dens  = ncvar_get(ncT, "Cohort_Density")[,7], # trees/m2
                             dbh   = ncvar_get(ncT, "Cohort_DBH")[,7], # DBH/tree
                             ba    = ncvar_get(ncT, "Cohort_BasalArea")[,7]) # cm2/m2
    
    # Calculate basal area: 
    # dat.cohort$ba <- ((dat.cohort$dbh)/2)^2 #gives squared radius
    # dat.cohort$ba <- (dat.cohort$ba)*pi*dat.cohort$dens 
    
    # Create placeholder columns for weighted variables:  
    dat.cohort$p.dens <- NA #
    dat.cohort$p.dbh <- NA 
    dat.cohort$dbh.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dbh, NA) #Create a column of DBH that only includes trees with a diameter above our threshold (10 cm). 
    dat.cohort$dens.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dens, NA)
    dat.cohort$ba.tree <- ifelse(dat.cohort$dbh>=10,dat.cohort$ba,NA)
    
    dat.cohort$p.dbh.tree <- NA # Creates a placeholder column for our weighted variables. 
    dat.cohort$p.dens.tree <- NA
    
    for(PCH in unique(dat.cohort$patch)){ #looks only at each patch based on the unique patchID
      for(PFT in unique(dat.cohort$pft)){ #looks at each PFT within each patchID
        row.ind <- which(dat.cohort$patch==PCH & dat.cohort$pft==PFT) # Row numbers for this group. 
        
        dat.tmp <- dat.cohort[row.ind,] # Subset our data to something small for our sanity. 
        dens.tot <- sum(dat.tmp$dens) # Sum of cohort densities by patch and PFT. 
        dens.tree <- sum(dat.tmp$dens.tree, na.rm=T) # Total density of the trees with a DBH above our threshold value. 
        
        # Weight density: 
        dat.tmp$p.dens <- dat.tmp$dens/dens.tot # Fractional density. 
        dat.tmp$p.dens.tree <- dat.tmp$dens.tree/dens.tree # Proportional density of trees with a DBH above our threshold. 
        
        # Weight dbh: 
        dat.tmp$p.dbh <- dat.tmp$dbh * dat.tmp$p.dens #Weights dbh by fractional density. 
        dat.tmp$p.dbh.tree <- dat.tmp$dbh.tree * dat.tmp$p.dens.tree
        
        dat.cohort[row.ind,c("p.dens", "p.dbh", "p.dens.tree", "p.dbh.tree")] <- dat.tmp[,c("p.dens", "p.dbh", "p.dens.tree", "p.dbh.tree")] # Put the new values into our table.
        
      }# Close PFT loop
    }# Close PCH loop
    
    dat.patch <- aggregate(dat.cohort[,c("agb","dens","p.dbh","dens.tree","p.dbh","dens.tree","p.dbh.tree","ba","ba.tree")],by=dat.cohort[,c("patch","pft")], FUN=sum, na.rm=T)
    dat.patch$dbh.max <- round(aggregate(dat.cohort$dbh, by=dat.cohort[,c("patch","pft")],FUN=max)[,"x"],2)
    names(dat.patch) <- car::recode(names(dat.patch), "'p.dbh'='dbh'; 'p.dbh.tree'='dbh.tree'")
    
    
    patch.area <- matrix(ncvar_get(ncT,"Patch_Area"),ncol=12)
    patch.area <- data.frame(patch.area)
    patch.area <- patch.area[,7]
    patch.area <- data.frame(patch=1:length(patch.area),# matches up patch ID with patch area
                             area=patch.area)
    
    dat.patch <- merge(dat.patch, patch.area, all.x=T) #merges the dataframes based on patch ID (column named patch)
    
    dat.patch[dat.patch$dens.tree==0, "dbh.tree"] <- NA
    dat.patch$p.agb <- dat.patch$agb * dat.patch$area
    dat.patch$p.dbh <- dat.patch$dbh * dat.patch$area
    dat.patch$p.dens <- dat.patch$dens * dat.patch$area
    dat.patch$p.ba <- dat.patch$ba * dat.patch$area
    dat.patch$p.dens.tree <- dat.patch$dens.tree * dat.patch$area
    dat.patch$p.ba.tree <- dat.patch$ba.tree * dat.patch$area
    
    # For trees, we need to weight by area of patches with TREES
    area.tree <- dat.patch[dat.patch$pft==10 & !is.na(dat.patch$dbh.tree),"area"]
    dat.patch$p.dbh.tree <- dat.patch$dbh.tree * dat.patch$area/sum(area.tree)
    
    dat.site <- aggregate(dat.patch[,c("p.agb","p.dens","p.dbh","p.ba","p.dens.tree","p.dbh.tree","p.ba.tree")], by=list(dat.patch$pft), FUN=sum, na.rm=T)
    dat.site$dbh.max <- aggregate(dat.patch[,"dbh.max"], by=list(dat.patch$pft), FUN=max)[,"x"]
    
    #Make the pft's more user-friendly: 
    colnames(dat.site)[1] <- "pft"
    dat.site <- subset(dat.site, subset=pft!=0)
    dat.site$pft <- car::recode(dat.site$pft, "'5'='Grasses'; '10'='Hardwoods'")
    
    # Add RUNID and year information: 
    dat.site <- data.frame(RUNID=RUNID,
                           year=i,
                           dat.site)
    
    #Add soil information
    soilmoist <- data.frame(ncvar_get(ncT300, "SoilMoist")[,7]) #Retrieves soil information for the growing season (July)
    # soilmoist is a datatable where each column represents a month and each row represents a depth. 
    
    slz <- c(-2.17, -1.50, -1.10, -0.80, -0.60, -0.45, -0.30, -0.20, -0.12, -0.06) #Gives absolute soil depth; organized from deepest to shallowest soil layer
    soilmoist <- soilmoist[8:10,] # Subset table because we are only concerned with the last three depths (topmost layers). 
    slz <- slz[8:10] # Subset vector because we are only concerned with the last three depths (topmost layers)
    
    # Create weights to generate a weighted average based on depth:
    p.top <- slz[3]/slz[1] #Divide shallow layer by total depth of all three layers (slz[1])
    p.middle <- (slz[2]-slz[3])/slz[1] #The second value in slz marks the bottom of the second layer. To get how thick it is, we have to subtract it from the depth of the top layer, and then to see how much of the soil depth it accounts for we have to divide it by the total depth. 
    p.bottom <- (slz[1]-slz[2])/slz[1]
    
    # Create the weighted averages using above proportions in lines 105-108 
    soilmoist_avg <- matrix(0, nrow=3, ncol=1)
    soilmoist_avg[1,] <- p.bottom*soilmoist[1]
    soilmoist_avg[2,] <- p.middle*soilmoist[2]
    soilmoist_avg[3,] <- p.top*soilmoist[3]
    soilmoist_avg<- colSums(soilmoist_avg)
    dat.site$soil_moist <- soilmoist_avg
    
    # Add a binary fire variable:
    fire <- matrix(ncvar_get(ncT,"Fire_flux"))
    if(sum(fire)!=0){
      fire <- "Yes" #Means that fire occured. 
    } else {
      fire <- "No" #Means that fire did not occur. 
    }
    dat.site$fire <- fire
    
    # Combine it into a new dataframe: 
    if(i==1 & RUNID==all.runs[1]){
      dat.out <- dat.site
    } else {
      dat.out <- rbind(dat.out, dat.site)
    }
    
    nc_close(ncT)
    
  }# Close i loop
}# Close RUNID loop

write.csv(dat.out,paste0("./output_runs_v4.csv"), row.names=F) # Writes the output to one csv. 
