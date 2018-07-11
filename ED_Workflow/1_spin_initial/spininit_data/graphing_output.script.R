##############
# AGB Graphs #
##############

library(ncdf4)

#The goal of this code is to produce a graph (or two) that compares the AGB of different plant functional types. 
#First we'll be working with one file, then we'll be working with multiple, until finally we can produce something with the entire server.

# -------------------------------------------- #
# Organizing the extracted output into a table #
# -------------------------------------------- #

#The goals for the following script are to get a table of AGB vs time. For now we can just graph the result in R,but since the server doesn't have a graphics card it would probably be ideal to write the data into a csv, then transfer the csv to my laptop and use another R script to produce the graphs. The csv will be large, but nowhere near as large as the raw data output, so it should all be good.  

path.nc <- "/Users/Cori/Research/Morton_Arb/Project/Die/"
files.nc <- dir(path.nc) #This will have to change, since I'll need to exclude the nc.var files somehow. I have some ideas, but I don't know if it'll work. But for now, this will suffice. 

test.agb <- nc_open(file.path(path.nc,files.nc[1]))
Table.agb <- data.frame(ncvar_get(test.agb,"Cohort_AbvGrndBiom"))
Table.pft <- data.frame(ncvar_get(test.agb,"Cohort_PFT"))
Table.pft_name <- data.frame(ncvar_get(test.agb,"PFT_name"))

#Okay. So I have a table with the aboveground biomass, and a table with the cohort PFTs. So I need to find some way to combine the cohorts a bit--that means I'm going to have to combine some rows--keep the columns separate. 

?aggregate

# -------------------------- #
# Graphing the extracted AGB #
# -------------------------- #

library(ggplot2)

#This will probably be moved to a separate code so that I can use one on the server and one on my laptop. It'll be fun. I hope. 

