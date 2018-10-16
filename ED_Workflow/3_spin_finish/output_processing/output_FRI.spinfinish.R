######################################
# Fire Return Intervals for Each Run #
######################################

# For each run, find out how many times fire occurred per decade: 

my_output <- read.csv("./output_runs_ALL.csv")
my_output <- subset(my_output, subset=my_output$pft=="Hardwoods")
my_output <- subset(my_output, subset=my_output$year>410)

RUNID <- c(unique(my_output$RUNID))
RUNID <- unique(as.character(my_output$RUNID))
class(RUNID)

for(s in RUNID){
  all_years <- subset(my_output,subset=my_output$RUNID==s)
  fire_years <- which(all_years$fire=="Yes")
  FRI <- (length(all_years$fire)/(length(fire_years)))
  FRI.df_temp <- data.frame(RUNID=s,
                            FRI=FRI)
  
  if(s=="CD-SC-FN-TN-IN"){
    FRI.df <- FRI.df_temp
  } else {
    FRI.df <- rbind(FRI.df,FRI.df_temp)
  } #Close ifelse statement
} # Close s loop
