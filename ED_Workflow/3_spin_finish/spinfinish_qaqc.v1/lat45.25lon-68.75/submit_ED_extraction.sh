#!/bin/sh
#$ -wd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/3_spin_finish/spinfinish_qaqc.v1/lat45.25lon-68.75
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -q "geo*"
#$ -N qaqc_lat45.25lon-68.75
#cd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/3_spin_finish/spinfinish_qaqc.v1/lat45.25lon-68.75
R CMD BATCH extract_output_general.R