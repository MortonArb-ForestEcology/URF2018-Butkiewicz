# Define pecan file path
# path.pecan <- "~/Desktop/Research/pecan"
path.pecan <- "~/pecan/"

# Source PEcAn ED conversion file
source(file.path(path.pecan, "base/utils/R/seconds_in_year.R"))
source(file.path(path.pecan, "base/utils/R/days_in_year.R"))
source(file.path(path.pecan, "modules/data.atmosphere/R/solar_angle.R"))
source(file.path(path.pecan, "models/ed/R/write_ed_metheader.R"))
source(file.path(path.pecan, "models/ed/R/check_ed_metheader.R"))
source("met2model.ED2.R")


in.base="~/met_ensemble/data/met_ensembles/GLSP.v1/1hr/ensembles"
outfolder="/home/models/ED_MET/GLSP.v1"
met.base="/home/models/ED_MET/GLSP.v1"
path.co2 = "/home/crollinson/ED_PalEON/MIP2_Region/phase2_env_drivers_v2/co2"
if(!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

ed.order <- read.csv("ExperimentalDesign.csv")

# Clean up the GCM strings
ed.order2 <- stringr::str_split(unique(ed.order$Climate), "_")
ed.order2 <- data.frame(matrix(unlist(ed.order2), ncol=length(ed.order2[[1]]), byrow=T))
names(ed.order2) <- c("GCM", "ens")
ed.order2$GCM <- as.factor(gsub("[.]", "-", ed.order2$GCM))
ed.order2$EnsID <- as.factor(paste(ed.order2$GCM, ed.order2$ens, sep="_"))
# summary(ed.order2)
# head(ed.order2)

# Convert Met Ensemble
for(i in 1:nrow(ed.order2)){
  met2model.ED2(in.path=file.path(in.base, ed.order2$GCM[i], ed.order2$EnsID[i]), 
                in.prefix=ed.order2$EnsID[i], 
                path_co2=path.co2,
                outfolder=file.path(outfolder, ed.order2$EnsID[i]), 
                header_folder = file.path(met.base, ed.order2$EnsID[i]),
                start_date="1800-01-01", 
                end_date="2015-12-31")
}
