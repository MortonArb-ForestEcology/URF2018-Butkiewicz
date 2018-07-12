###############################
# Above Ground Biomass Graphs #
###############################

# ----------------------- #
# Formatting a Data Table #
# ----------------------- #

library(ncdf4)
library(modeest)

#This is stored in and run from the "spininit_data" folder that I made. 
path.nc <- "../extracted_output/CD-SC-FN-TN-IN" #This has to change on my computer, but y'know. 
files.nc <- dir(path.nc)

for(i in 1:length(files.nc)){
  if(i==1){
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
    
    agb.data <- rbind(agb.grasses,agb.trees)
    
    days <- c(test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals)
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
    
    agb.data_temp <- rbind(agb.grasses,agb.trees)
    
    days <- c(test.nc$var$Cohort_AbvGrndBiom$dim[[4]]$vals+days[length(days)])
    agb.data_temp$days <- days
    
    agb.data <- rbind(agb.data,agb.data_temp)
  }
  print(i)
}
rownames(agb.data) <- c(1:length(agb.data$agb))
write.csv(agb.data,file="/Users/Cori/Desktop/Did_it_work?") #The hope is that this won't take too too much space on my computer, so I can download the files and make graphs out of them. 

# ----------------- #
# Graphing the Data #
# ----------------- #

library(ggplot2)

plot <- ggplot(agb.data,aes(x=days,y=agb,color=pft))+geom_line()+xlab("Days since 01-01-1801")+ylab("Above Ground Biomass, kg C m-2")+theme(panel.background=element_rect(fill="white"))+theme_bw()+theme(panel.grid=element_blank())+scale_color_manual(values=c("#E69F00","#009E73"))
plot 
