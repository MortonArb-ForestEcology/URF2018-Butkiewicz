#!/bin/sh
#$ -wd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/1_spin_initial/spininit_qaqc.v1/lat46.25lon-94.75
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -q "geo*"
#$ -N qaqc_lat46.25lon-94.75
#cd /projectnb/dietzelab/paleon/ED_runs/MIP2_Region/1_spin_initial/spininit_qaqc.v1/lat46.25lon-94.75
R CMD BATCH extract_output_general.R