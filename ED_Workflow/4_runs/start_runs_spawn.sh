#!/bin/bash
# This file takes the ending point for the spin finish (transient runs) and starts the 
# full PalEON Simulations
# Christy Rollinson, crollinson@gmail.com
# January, 2016

# Order of operations for starting the spin finish after having run the SAS script
# 1. Copy & rename last output file from transient runs
# 2. Copy ED2IN, executable, submission script, and xml file to new location
# 3. Change file paths, init mode, & turn on disturbance in ED2IN

# Things to change from the spin initial
#   all fille paths from initial spin to final spin
#   run title = Spin Finish (Post-SAS) (NL%EXPNME)
#   init mode = 5 (history file; ignoring the descriptions as they don't line up
#               with my understanding of the different innitialization methods)
#   Modify run date
#   met dates 

# Load the necessary hdf5 library
# module load hdf5/1.6.10

# Define constants & file paths for the scripts
file_base=~/URF2018-Butkiewicz/ED_Workflow # whatever you want the base output file path to be

ed_exec=/home/models/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
spin_dir=${file_base}/3_spin_finish/URF2018_spinfinish.v1/ # Directory of initial spin files
runs_dir=${file_base}/4_runs/URF2018_runs.v1/ # Where the transient runs will go
setup_dir=${file_base}/0_setup/
finalspin=2351 # The last year of the spin finish
finalrun=2015 # The last full year of the runs

USER=crolli

n=1 # number of RUNs to start in this batch

# Making the file directory if it doesn't already exist
mkdir -p $runs_dir

# Get the list of what grid runs already have at least started full runs
pushd $runs_dir
	file_done=(lat*)
popd

# Get the list of what grid runs have at least started the spin finish
pushd $spin_dir
	runs=(lat*)
popd


# This will probably be slow later on, but will probably be the best way to make sure we're
# not skipping any RUNs
# NOTE: NEED TO COMMENT THIS PART OUT FIRST TIME THROUGH 
#       because it doesn't like no matches in file_done
for REMOVE in ${file_done[@]}
do 
	runs=(${runs[@]/$REMOVE/})
done

# Filter RUNs that have successfully complete the spinfinish
for RUN in ${runs[@]}
do
	#get dates of last histo file
    path=${spin_dir}${RUN}
    lastday=`ls -l -rt ${path}/histo| tail -1 | rev | cut -c15-16 | rev`
    lastmonth=`ls -l -rt ${path}/histo| tail -1 | rev | cut -c18-19 | rev`
    lastyear=`ls -l -rt ${path}/histo| tail -1 | rev | cut -c21-24 | rev`

	# If the last year isn't the last year of the spin finish, don't do it for now
	if [[(("${lastyear}" < "${finalspin}"))]]
	then
		echo "  RUN not done: $RUN"
		runs=(${runs[@]/$RUN/})
	fi
done

for ((FILE=0; FILE<$n; FILE++)) # This is a way of doing it so that we don't have to modify N
do
	# RUN Name and Lat/Lon
	RUN=${runs[FILE]}
	echo $RUN
	
	# Make a new folder for this RUN
	file_path=${runs_dir}/${RUN}/
	mkdir -p ${file_path} 
	
	pushd ${file_path}
		# Creating the default file structure and copying over the base files to be modified
		mkdir -p histo analy
		ln -s $ed_exec
		
		# Copy the essential run files
		cp ${spin_dir}${RUN}/ED2IN .
		cp ${spin_dir}${RUN}/PalEON_Phase2.v1.xml .
		
		# copy the files to automatically restart
		cp ${setup_dir}spawn_startloops.sh . 

		# copy the files that try a smaller timestep if we crash
		cp ${setup_dir}adjust_integration_restart.sh . 
		
	    # Copy the last January histo so we start at the appropriate phenological state
	    lastday=`ls -l -rt ${spin_dir}${RUN}/histo| tail -1 | rev | cut -c15-16 | rev`
	    lastmonth=`ls -l -rt ${spin_dir}${RUN}/histo| tail -1 | rev | cut -c18-19 | rev`
	    lastyear=`ls -l -rt ${spin_dir}${RUN}/histo| tail -1 | rev | cut -c21-24 | rev`

		echo "     Last Spin Finish Year = $lastyear"
		if [[(("${lastmonth}" > 01))]]
		then
			lastyear=$(($lastyear-1))
		fi
		echo "     Use Spin Finish Year = $lastyear"

		cp ${spin_dir}${RUN}/histo/*-S-$lastyear-01-01-* histo/${RUN}-S-1800-01-01-000000-g01.h5 

		# ED2IN Changes	    
	    sed -i "s,$spin_dir,$runs_dir,g" ED2IN #change the baseline file path everywhere
        sed -i "s/NL%EXPNME =.*/NL%EXPNME = 'URF Runs'/" ED2IN # change the experiment name
        sed -i "s/NL%RUNTYPE  = 'INITIAL'.*/NL%RUNTYPE  = 'HISTORY'/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s/NL%IED_INIT_MODE   = .*/NL%IED_INIT_MODE   = 5/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s,SFILIN   = .*,SFILIN   = '${runs_dir}${RUN}/histo/${RUN}',g" ED2IN # set initial file path to the SAS spin folder
        sed -i "s/NL%IYEARA   = .*/NL%IYEARA   = 1800/" ED2IN # Set runs start year
        sed -i "s/NL%IMONTHA  = .*/NL%IMONTHA  = 01/" ED2IN # Set runs start month
        sed -i "s/NL%IDATEA   = .*/NL%IDATEA   = 01/" ED2IN # Set runs start day
        sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = 2015/" ED2IN # Set runs last year
        sed -i "s/NL%IMONTHZ  = .*/NL%IMONTHZ  = 01/" ED2IN # Set runs last month
        sed -i "s/NL%IDATEZ   = .*/NL%IDATEZ   = 01/" ED2IN # Set runs last day
        sed -i "s/NL%IYEARH   = .*/NL%IYEARH   = 1800/" ED2IN # Set histo year
        sed -i "s/NL%IMONTHH  = .*/NL%IMONTHH  = 01/" ED2IN # Set histo month
        sed -i "s/NL%IDATEH   = .*/NL%IDATEH   = 01/" ED2IN # Set histo day
        sed -i "s/NL%METCYC1     =.*/NL%METCYC1     = 1800/" ED2IN # Set met start
        sed -i "s/NL%METCYCF     =.*/NL%METCYCF     = 2015/" ED2IN # Set met end


		# spawn restarts changes
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" spawn_startloops.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalrun}/" spawn_startloops.sh 		
		sed -i "s,sub_post_process.sh,sub_post_process_runs.sh,g" spawn_startloops.sh 
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops.sh # set the file path

		# adjust integration step changes
		sed -i "s/USER=.*/USER=${USER}/" adjust_integration_restart.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" adjust_integration_restart.sh 		

		# copy & edit the files that clean up the previous step
		cp ${setup_dir}cleanup_spinfinish.sh .
	    sed -i "s,/DUMMY/PATH,${spin_dir}${RUN}/,g" cleanup_spinfinish.sh # set the file path
		sed -i "s/RUN=.*/RUN=${RUN}/" cleanup_spinfinish.sh 		
	    sed -i "s/lastyear=.*/lastyear=${finalspin}/" cleanup_spinfinish.sh 		

		# copy & edit the files that do all of the post-run housekeeping
		cp ../../post_process_runs.sh .
		cp ${setup_dir}submit_ED_extraction.sh .
		cp ${setup_dir}extract_output_paleon.R .
		cp ${setup_dir}cleanup_runs.sh .
		paleon_out=${file_path}/${RUN}_paleon		

		sed -i "s/RUN=.*/RUN=${RUN}/" post_process_runs.sh 		
		sed -i "s/job_name=.*/job_name=extract_${RUN}/" post_process_runs.sh 		
		sed -i "s,/dummy/path,${paleon_out},g" post_process_runs.sh # set the file path

		sed -i "s/RUN=.*/RUN='${RUN}'/" extract_output_paleon.R
	    sed -i "s,/dummy/path,${file_path},g" extract_output_paleon.R # set the file path

		sed -i "s,/DUMMY/PATH,${file_path}/,g" cleanup_runs.sh # set the file path
		sed -i "s/RUN=.*/RUN=${RUN}/" cleanup_runs.sh
		sed -i "s/lastyear=.*/lastyear=3011/" cleanup_runs.sh

	popd

	chmod -R a+rwx ${file_path}

done
