#!/bin/sh
#$ -wd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/4_runs/runs_qaqc.v1/lat46.25lon-89.75
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -q "geo*"
#$ -N qaqc_lat46.25lon-89.75
#cd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/4_runs/runs_qaqc.v1/lat46.25lon-89.75
R CMD BATCH extract_output_general.R