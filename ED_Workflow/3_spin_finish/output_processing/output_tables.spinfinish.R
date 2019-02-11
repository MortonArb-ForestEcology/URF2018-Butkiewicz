###########################
# Spinfinish Output Table #
###########################

# This script is meant to compile data from the Step 3 (spin finish) extracted output into two tables: one master table containing
# basal area, density, aboveground biomass, pft, and fire frequency while the other just includes fire frequencies per scenario for 
# ease of managing data. 

# Load appropriate packages into library
library(ncdf4)
library(car)

all.runs <- dir("../extracted_output.v5/") # Stores all directories within filepath. 

for(RUNID in all.runs){ #Looks at each individual RUNID (scenario). 
  path.nc <- file.path("../extracted_output.v5",RUNID) #Set up file paths for each RUNID.
  files.nc <- dir(path.nc,"ED2") #Stores files in  RUNID folder, one file per year. 
  print(paste0("----- Processing run: ", RUNID)) # Keep track of where function is currently working. 
  
  for(y in 1:length(files.nc)){ #looking at one year at a time
    print(paste0("*** Processing year: ", 2199+y)) # Keep track of where function is currently working. 
    ncT <- nc_open(file.path(path.nc,files.nc[y])) # Open extracted output. 
    
    # ---------------------------
    # Retrieve cohort-level data
    # ---------------------------
    
    # Cohort data is organized into table where row reprsents each cohort and column represents each month.
    # Extract data cohort-level data from seventh row (July) for growing season data. Each row represents a different cohort. 
    dat.cohort <- data.frame(RUNID=RUNID,
                             year=y,
                             patchID = ncvar_get(ncT, "Cohort_PatchID")[,7], #Identifies each cohort based on patch. 
                             pft   = ncvar_get(ncT, "Cohort_PFT")[,7], 
                             agb   = ncvar_get(ncT, "Cohort_AbvGrndBiom")[,7], # kgC/m2
                             dens  = ncvar_get(ncT, "Cohort_Density")[,7], # trees/m2
                             dbh   = ncvar_get(ncT, "Cohort_DBH")[,7], # DBH/tree
                             ba    = ncvar_get(ncT, "Cohort_BasalArea")[,7]) # cm2/m2
    # Create additional columns that exclusively includes output from trees with DBH > 10 cm. 
    dat.cohort$dbh.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dbh, NA) 
    dat.cohort$dens.tree <- ifelse(dat.cohort$dbh>=10, dat.cohort$dens, NA)
    dat.cohort$ba.tree <- ifelse(dat.cohort$dbh>=10,dat.cohort$ba,NA)
    
    # --------------------------------------------------------------------------------------------------------------------
    # Aggregate data from each cohort into a patch-level dataframe by averaging the output in dat.cohort based on density. 
    # --------------------------------------------------------------------------------------------------------------------
    
    # First weight each cohort by density and create placeholder columns for weighted variables: 
    dat.cohort$w.dbh <- NA # weighted diameter at breast height
    dat.cohort$w.ba <- NA # weighted basal area
    dat.cohort$w.dbh.tree <- NA # weighted dbh of trees >10 cm
    dat.cohort$w.ba.tree <- NA # weighted basal area of trees >10 cm
    # Note that density is not weighted. It will be directly summed instead of averaged. 
    
    for(PCH in unique(dat.cohort$patchID)){ # Look only at each patch based on unique patchID
      for(PFT in unique(dat.cohort$pft)){ # Look at each PFT within patch
        row.ind <- which(dat.cohort$patchID==PCH & dat.cohort$pft==PFT) # Row numbers for this group. 
        dat.tmp <- dat.cohort[row.ind,] # Subset dataframe based on patchID and pft 
        
        # Calculating density weight
        dens.tot <- sum(dat.tmp$dens) # Sum cohort densities by patch and PFT. 
        dens.tot.tree <- sum(dat.tmp$dens.tree, na.rm=T) # Total density of trees with DBH >10 cm 
        dat.tmp$wght <- dat.tmp$dens/dens.tot # Assigns weight to each cohort based on desnity
        dat.tmp$wght.tree <- dat.tmp$dens.tree/dens.tot.tree # Assigns weight to each cohort with trees >10 cm based on density
        
        # Weight the variables
        dat.tmp$w.dbh <- dat.tmp$dbh*dat.tmp$wght # Weighted dbh
        dat.tmp$w.dbh.tree <- dat.tmp$dbh.tree*dat.tmp$wght.tree # Weighted dbh for trees >10 cm

        dat.cohort[row.ind,c("w.agb","w.dbh","w.ba","w.dbh.tree","w.ba.tree")] <- dat.tmp[,c("w.agb","w.dbh","w.ba","w.dbh.tree","w.ba.tree")] # Put the weighted variables into the columns assigned for weighted variables in lines 48-52. 
        
      } #End PFT loop. 
    } # End PCH loop. 
    
    # AGGREGATE INTO PATCH-LEVEL DATA
    # Note that aboveground biomass (agb), total plant density (dens), tree density (dens.tree), total basal area (ba), and tree basal
    # area (ba.tree) are simply summed because they are per area and each cohort individually contributes to overall patch data. 
    dat.patch <- aggregate(dat.cohort[,c("agb","dens","dens.tree","w.dbh","w.dbh.tree","ba","ba.tree")],by=dat.cohort[,c("patchID","pft")], FUN=sum, na.rm=T)
    names(dat.patch) <- car::recode(names(dat.patch), "'w.dbh'='dbh'; 'w.dbh.tree'='dbh.tree'") # Recodes weighted DBH (w.dbh) as regular (or average) dbh. 
    
    
    # -------------------------------
    # Aggregate into site-level data 
    # -------------------------------
    
    patch.area <- matrix(ncvar_get(ncT,"Patch_Area"),ncol=12) # fraction of total area occupied by each patch
    patch.area <- data.frame(patch.area) 
    patch.area <- patch.area[,7]
    patch.area <- data.frame(patchID=1:length(patch.area),# matches up patch ID with patch area
                             area=patch.area)
    
    dat.patch <- merge(dat.patch, patch.area, all.x=T) #merges the dataframes based on patchID
    dat.patch[dat.patch$dens.tree==0, "dbh.tree"] <- NA
    
    # Create columms for output weighted by patch area
    dat.patch$w.agb <- dat.patch$agb * dat.patch$area
    dat.patch$w.dbh <- dat.patch$dbh * dat.patch$area
    dat.patch$w.dens <- dat.patch$dens * dat.patch$area
    dat.patch$w.ba <- dat.patch$ba * dat.patch$area
    dat.patch$w.ba.tree <- dat.patch$ba.tree * dat.patch$area
    
    # For trees, we need to weight by area of patches with TREES
    area.tree <- dat.patch[dat.patch$pft==10 & !is.na(dat.patch$dbh.tree),"area"]
    dat.patch$w.dens.tree <- dat.patch$dens.tree * dat.patch$area/sum(area.tree)
    dat.patch$w.dbh.tree <- dat.patch$dbh.tree * dat.patch$area/sum(area.tree)
    dat.patch$w.ba.tree <- dat.patch$ba.tree * dat.patch$area/sum(area.tree)
    
    #Aggregate everything into site-level dataframe. 
    dat.site <- aggregate(dat.patch[,c("w.agb","w.dens","w.dbh","w.ba","w.dens.tree","w.dbh.tree","w.ba.tree")], by=list(dat.patch$pft), FUN=sum, na.rm=T)
    
    #Make the pft's more user-friendly: 
    colnames(dat.site)[1] <- "pft"
    dat.site <- subset(dat.site, subset=pft!=0)
    dat.site$pft <- car::recode(dat.site$pft, "'5'='Grasses'; '10'='Hardwoods'")
    
    # Add RUNID and year information: 
    dat.site <- data.frame(RUNID=RUNID,
                           year=y-1,
                           dat.site)
    
    #Add soil information
    soilmoist <- data.frame(ncvar_get(ncT, "SoilMoist")[,7]) #Retrieves soil information for the growing season (July)
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
      fire <- 1 #Means that fire occured. 
    } else {
      fire <- 0 #Means that fire did not occur. 
    }
    dat.site$fire <- fire
    
    # Combine it into a new dataframe: 
    if(i==1 & RUNID==all.runs[1]){
      dat.out <- dat.site
    } else {
      dat.out <- rbind(dat.out, dat.site)
    }
    
    nc_close(ncT)
    
    } # End y loop. 
} # End RUNID loop. 

# --------------------------------
# Calculate fire return intervals. 
# --------------------------------

dat.sub <- subset(dat.out, subset=dat.output$pft=="Hardwoods") #Soft-coded: hardwoods tend to appear in every year, in every run.

RUNID <- c(unique(dat.sub$RUNID))
RUNID <- unique(as.character(dat.sub$RUNID))
class(RUNID)

for(n in RUNID){
  RUNID_temp <- subset(dat.sub, subset=dat.sub$RUNID==n) # Subset table based on runID
  FRI <- length(RUNID_temp)/sum(dat.sub$fire) #Divides years in dataset by the number of times fire occurred.
  FRI.df_temp <- data.frame(RUNID=n,
                            FRI=FRI)
  if(n==RUNID[1]){
    FRI.df <- FRI.df_temp
  } else {
    FRI.df <- rbind(FRI.df,FRI.df_temp)
  } #Close ifelse statement
} # Close n loop

dat.site <- merge(dat.site, FRI.df) # Add fire return intervals to table

write.csv(FRI.df,paste0("/Users/Cori/Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_FRI_v4.csv"), row.names=F) # Writes fire return interval to 
write.csv(dat.out,paste0("./output_runs_v5.csv"), row.names=F) # Writes site-level output to separate csv. 
