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

# Then average June, July and August. 
y=1800
averages <- NULL
while(y<=1829){
  print(y) 
  tempdf <- PDSI.GLSP_29yr_JJA[which(PDSI.GLSP_29yr_JJA$year==y),]
  averages <- rbind(averages,colMeans(tempdf))
  y=y+1
}
summary(averages)
averages <- data.frame(averages)

#Average the columns across each year.
averages_yr <-data.frame(colMeans(averages))
averages_yr.test <- averages_yr
averages_yr <- averages_yr[-c(202,203),]


# Find the column that has the minimum average value for June, July and August. 
