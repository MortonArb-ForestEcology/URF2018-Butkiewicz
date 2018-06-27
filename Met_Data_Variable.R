PDSI.GLSP <- read.csv("./PDSI_AllMembers.csv") #Reading in the CSV, and called it PDSI (for the name of the drought index) .GLSP (for Glacial Lakes state park).
class(PDSI.GLSP)
ncol(PDSI.GLSP) #This should tell me the number of columns. I want to add a new column, so that's how this will work. There should be 201 columns. 
# Each column is a different weather. 

# Add columns that describe the year, month, and day. Row 1=January 1 1800. 
PDSI.GLSP$month <- rep(1:12,length.out=nrow(PDSI.GLSP))
ncol(PDSI.GLSP)
summary(PDSI.GLSP$month)

PDSI.GLSP$year <- rep(1800:(1800-1+nrow(PDSI.GLSP)/12), length.out=nrow(PDSI.GLSP), each=12)
ncol(PDSI.GLSP)
summary(PDSI.GLSP$year)

# Subset the table for only 1800-1829. 

#Does NOT work!!!
PDSI.GLSP_29yr <- subset(PDSI.GLSP,subset=year<=1829)

#The following just checks to make sure that the column is correct. 
summary(PDSI.GLSP_29yr) #Just to make sure all of the columns are in order. 
nrow(PDSI.GLSP_29yr) #Should be 360. 
min(PDSI.GLSP_29yr$year) #Should be 1800. 
max(PDSI.GLSP_29yr$year) #Should be 1829.
PDSI.GLSP_29yr$year #Should print a list, the first number being 1800 and the last being 1829, which should each be repeated 12 times. 

# Subset the table for June, July, and August. 
PDSI.GLSP_29yr_JJA <- subset(PDSI.GLSP_29yr,subset=month>=6)
PDSI.GLSP_29yr_JJA$month
min(PDSI.GLSP_29yr_JJA$month) #Should be 6
PDSI.GLSP_29yr_JJA <- subset(PDSI.GLSP_29yr_JJA,subset=month<=8)
PDSI.GLSP_29yr_JJA$month
max(PDSI.GLSP_29yr_JJA$month)

# Then average June, July and August to get the average PDSI values during the growing season.  The next chunk of code is super sloppy so I'll just stick lots of comments on here.

y=1800
averages <- NULL #I created essentially an empty dataframe to put things in. 
while(y<=1829){
  print(y) #This is just here to make sure that it's iterating through the years. 
  tempdf <- PDSI.GLSP_29yr_JJA[which(PDSI.GLSP_29yr_JJA$year==y),] #A temporary dataframe that gets overwritten constantly. By the end of this loop it should only have 3 rows and 203 columns. 
  averages <- rbind(averages,colMeans(tempdf)) 
  y=y+1
}
summary(averages) #A check to make sure this did what I wanted it to do. 
averages <- data.frame(averages)
nrow(averages) #Since there's only 30 years in the dataset there should only be 30 rows in averages, one for each year. 


#Average the columns across the years to get the total average PDSI during the growing season from 1800-1829 for each weather. Right now everything is stored in a new variable. If I get a chance to clean this up the variables will be overwritten, but for now this works too. 

averages_yr <-data.frame(colMeans(averages)) #I took the averages of all the columns in averages. 
rownames(averages_yr)
averages_yr.PDSI <- averages_yr[-c(1,202,203),] #A bit of soft-coding here, I knew the row numbers I wanted to delete. This should delete the columns labeled "X", "month", and "year".
averages_yr.rownames <- rownames(averages_yr) #I couldn't figure a way to get a data frame with what I had, so I made two vectors, one with the desired columns and one with the desired rows. 
averages_yr.rownames #Just to make sure that everything's here. 
averages_yr.rownames <- averages_yr.rownames[-c(1,202,203)] #Subset the vector. 
averages_yr.rownames #Check again. 
averages_yr.test <- data.frame(weather=averages_yr.rownames,PDSI=averages_yr.PDSI) 
averages_yr.test

#Find the column with the maximum average PDSI for June, July, and August--the wettest weather. 

rownames(averages_yr.test) <- averages_yr.rownames
averages_yr.test #Should give you a column labeled "weather" and a column labeled "PDSI"
max(averages_yr.test$PDSI)
max_average_weather <- subset(averages_yr.test,subset=PDSI==max(averages_yr.test$PDSI))
max_average_weather #Should print the maximum average PDSI value found in the table and the weather associated with it. 

# Find the column that has the minimum average value for June, July and August--the driest weather.

min(averages_yr.test$PDSI)
min_average_weather <- subset(averages_yr.test,subset=PDSI==min(averages_yr.test$PDSI))
min_average_weather #Should print the minimum average PDSI value found in the table and the weather associated with it. 
