######################################
# Fire Return Intervals for Each Run #
######################################

# The purpose of this code is to get a fire return interval for each run. 

my_output <- read.csv("./output_runs_ALL.csv")
my_output <- subset(my_output, subset=my_output$pft=="Hardwoods") #Hardwoods survive in every ecosystem, and I don't need two rows per run. 
my_output <- subset(my_output, subset=my_output$year>410) # Only look at last ninety years. 

##########################
# LOW MOISTURE THRESHOLD: 
##########################

###########################
# HIGH MOISTURE THRESHOLD: 
###########################