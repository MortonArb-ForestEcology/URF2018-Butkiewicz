# Get Experimental Design from Google Drive and save it as a csv as a key for our ED runs

library(googlesheets)

# Register the Experimental Design Google sheet
# Note: If doing this for the first time, it will open your browser and 
#       you'll need to enter a key
exp.design.wb <- gs_title("URF 2018 Experimental Design")
exp.design.wb # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
exp.design <- data.frame(gs_read(exp.design.wb, ws="ExpDesign"))
summary(exp.design)

# Put the treatments in the order we want to do them
exp.design <- exp.design[order(exp.design$order),]

exp.design

write.csv(exp.design, file="../ED_Workflow/0_setup/ExperimentalDesign.csv", na="", row.names=F, quote = F, fileEncoding="utf8")
