# URF2018-Butkiewicz
Summer Undergrad: Prairie-Forest Boundary dynamics

We need to add a readme that includes information on 
1. What this project is about
2. How to run/use code & products

Christy's best example: https://github.com/PalEON-Project/modeling_met_ensemble

This repository contains an R-based workflow for ED2 as well as code for determining project-specific variables for ED2 spin-ups. This is a factorial experiment that will test the effects of different combinations of climate, soil moisture, and fire thresholds on the prairie-forest ecotone.  

IN THE FOLDER "SCRIPTS""

There should be three sets of code: Get_ExpDesign.R, Met_Data_Variable.R, and SMFIRE_Calcs_ED.R. These do not need to be implemented for the project to run smoothly; rather, they are a set of scripts we used to refine our experimental design. 

Get_ExpDesign.R is script we used to retrieve our experimental design from Google Drive and save it as a csv so that we could use it in our ED runs. This should be saved in the directory ./ED_Workflow/0_setup/ExperimentalDesign.csv. 

Met_Data_Variable.R is script used to determine the climate variable in ExperimentalDesign.csv. I was given the PDSI (Palmer Severity Drought Index) values for 200 different climates from AD 1800-2015. The script was designed to subset the years 1800-1829 (which was the 30 years of climate we will use for our spin-ups) and then to determine which of the 200 climates had the highest and lowest average PDSI for the growing season across all 30 years. The variables given at the end of the code should be max_average_weather and min_average_weather; the former provides the wettest climate, while the latter provides the driest climate. 

SMFIRE_Calcs_ED.R provides some sample calculations for how SM_Fire is determined. This script helped us decide that SM_FIRE should be > 0. 