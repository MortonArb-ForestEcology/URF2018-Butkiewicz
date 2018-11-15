######################################
# Fire Return Intervals for Each Run #
######################################

# For each run, find out how many times fire occurred per decade: 

my_output <- read.csv("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_runs_v4.csv")
my_output <- subset(my_output, subset=my_output$pft=="Hardwoods") #Soft-coded: hardwoods tend to appear in every year. 
# my_output <- subset(my_output, subset=my_output$year>410)

RUNID <- c(unique(my_output$RUNID))
RUNID <- unique(as.character(my_output$RUNID))
class(RUNID)

for(s in RUNID){
  all_years <- subset(my_output,subset=my_output$RUNID==s)
  fire_years <- which(all_years$fire=="Yes")
  FRI <- (length(all_years$fire)/(length(fire_years)))
  FRI.df_temp <- data.frame(RUNID=s,
                            FRI=FRI)
  
  if(s==RUNID[1]){
    FRI.df <- FRI.df_temp
  } else {
    FRI.df <- rbind(FRI.df,FRI.df_temp)
  } #Close ifelse statement
} # Close s loop

write.csv(FRI.df,paste0("../../Research/Forests_on_the_Edge/URF 2018 Butkiewicz/Project_Output/v4/output_FRI_v4.csv"), row.names=F)
