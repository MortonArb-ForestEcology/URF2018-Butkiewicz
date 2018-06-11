#!/bin/sh
#$ -wd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/4_runs/runs_qaqc.v1/lat42.75lon-72.25
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -q "geo*"
#$ -N qaqc_lat42.75lon-72.25
#cd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/4_runs/runs_qaqc.v1/lat42.75lon-72.25
R CMD BATCH extract_output_general.R