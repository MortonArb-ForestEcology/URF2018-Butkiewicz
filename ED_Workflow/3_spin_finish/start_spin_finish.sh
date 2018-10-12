#!bin/bash
# This file runs the transient runs post SAS steady-state calculations to get to true(er)
# steady-state conditions
# Christy Rollinson, crollinson@gmail.com
# January, 2016

# Order of operations for starting the spin finish after having run the SAS script
# 1. get list of RUNs that have an SAS init folder and use that to figure out what we're
#    need to run a spin finish (transient runs) on 
# 2. copy the basic files for an ED run (ED2IN, link to executable, .xml file) to the new 
#    folder
# 3. Change file paths, init mode, & turn on disturbance in ED2IN

# Things to change from the spin initial
#   run title = Spin Finish (Post-SAS) (NL%EXPNME)
#   init mode = 3 (uses .css & .pss files; ignoring the descriptions as they don't line up
#               with my understanding of the different innitialization methods)
#   turn on disturbance (fire, treefall)
#   all fille paths from initial spin to final spin

# Load the necessary hdf5 library
# module load hdf5/1.6.10

# Define constants & file paths for the scripts
# Note: do not need to re
file_base=~/URF2018-Butkiewicz/ED_Workflow # whatever you want the base output file path to be

ed_exec=/home/models/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
init_dir=${file_base}/1_spin_initial/URF2018_spininit.v4/ # Directory of initial spin files
SAS_dir=${file_base}/2_SAS/SAS_init_files.v4/ # Directory of SAS initialization files
finish_dir=${file_base}/3_spin_finish/URF2018_spinfinish.v4/ # Where the transient runs will go
setup_dir=${file_base}/0_setup/

setup_dir=${file_base}/0_setup/ # Where some constant setup files are
RUN_file=${setup_dir}/ExperimentalDesign.csv # # Path to list of ED RUNs w/ status

finalyear=2301 # The year on which the models should top on Jan 1
finalfull=2300 # The last year we actually care about (probably the year before finalyear)
finalinit=1800

n=1

# Making the file directory if it doesn't already exist
mkdir -p $finish_dir

# Getting a list of all runs
runs=($(awk -F ',' 'NR>1 {print $2}' ${RUN_file}))
inc_fire=2 # INCLUDE_FIRE
sm_fire=($(awk -F ',' 'NR>1 {print $7}' ${RUN_file})) # SM_FIRE
fire_int=($(awk -F ',' 'NR>1 {print $9}' ${RUN_file})) # FIRE_INTENSITY


# Get the list of what grid runs have already finished spinups
pushd $finish_dir
	file_done=(C*)
popd
file_done=(${file_done[@]/"s*"/})

# Get the list of what grid runs have SAS solutions
pushd $SAS_dir
	SAS=(s*)
popd


# This will probably be slow later on, but will probably be the best way to make sure we're
# not skipping any RUNs
# NOTE: NEED TO COMMENT THIS PART OUT FIRST TIME THROUGH 
#       because it doesn't like no matches in file_done

# NOTE: Need to adjust this to parse down the fire parameters as well
if((${#file_done[@]} > 0)); then
	for REMOVE in ${file_done[@]}
	do 
# 		runs=(${runs[@]/$REMOVE/})
		SAS=(${SAS[@]/$REMOVE/})
	done
fi

# Because we want to preserve the order of runs, I can't find away around doing a loop
# - This is slower than other options, but makes sure we still do our controls first
# - DO NOT imitate this with a large array
spinfin=()
fire_fin=()
smfire_fin=()
fireint_fin=()

for((i=0;i<${#runs[@]};i++)); do 
	RUN=${runs[i]}
    for FILE in "${SAS[@]}"; do
        if [[ $RUN == "$FILE" ]]; then
            spinfin+=("$RUN")
#             fire_fin+=("${inc_fire[i]}")
            smfire_fin+=("${sm_fire[i]}")
            fireint_fin+=("${fire_int[i]}")
        fi
    done
done

n=$(($n<${#spinfin[@]}?$n:${#spinfin[@]}))

for ((FILE=0; FILE<$n; FILE++)) # This is a way of doing it so that we don't have to modify N
do
	# RUN Name and Lat/Lon
	RUN=${spinfin[FILE]}
	INC_FIRE=$inc_fire
	SM_FIRE=${smfire_fin[FILE]}
	FIRE_INT=${fireint_fin[FILE]}
	
	echo $RUN
	
	# Make a new folder for this RUN
	file_path=${finish_dir}/${RUN}/
	mkdir -p ${file_path} 

	pushd ${file_path}
		# Creating the default file structure and copying over the base files to be modified
		mkdir -p histo analy
		ln -s $ed_exec
		cp ${init_dir}${RUN}/ED2IN .
		cp ${setup_dir}PFTParams_URF2018.xml .

		# ED2IN Changes	    
	    sed -i "s,$init_dir,$finish_dir,g" ED2IN #change the baseline file path everywhere
        sed -i "s/NL%EXPNME =.*/NL%EXPNME = 'URF Spin Finish'/" ED2IN # change the experiment name
		sed -i "s/NL%RUNTYPE  = .*/NL%RUNTYPE  = 'INITIAL'/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s/NL%IYEARA   = .*/NL%IYEARA   = 1800/" ED2IN # Set first year
        sed -i "s/NL%IMONTHA  = .*/NL%IMONTHA  = 06/" ED2IN # Set first month
        sed -i "s/NL%IDATEA   = .*/NL%IDATEA   = 01/" ED2IN # Set first day
		sed -i "s/NL%IYEARH   = .*/NL%IYEARH   = 1800/" ED2IN # Set first year
		sed -i "s/NL%IMONTHH  = .*/NL%IMONTHH  = 06/" ED2IN # Set first month
		sed -i "s/NL%IDATEH   = .*/NL%IDATEH   = 01/" ED2IN # Set first day

        sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = ${finalyear}/" ED2IN # Set last year
        sed -i "s/NL%IMONTHZ  = .*/NL%IMONTHZ  = 01/" ED2IN # Set last month
        sed -i "s/NL%IDATEZ   = .*/NL%IDATEZ   = 01/" ED2IN # Set last day

        sed -i "s/NL%IED_INIT_MODE   = .*/NL%IED_INIT_MODE   = 6/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s,SFILIN   = .*,SFILIN   = '${SAS_dir}${RUN}/${RUN}',g" ED2IN # set initial file path to the SAS spin folder
        sed -i "s/NL%DTLSM  .*/NL%DTLSM   = 480/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s/NL%RADFRQ .*/NL%RADFRQ  = 480/" ED2IN # set initial file path to the SAS spin folder
        
        
        # Change Fire & Disturbance Params HERE
        # Note: Already set other params, so we just need to match the 
        sed -i "s/NL%TREEFALL_DISTURBANCE_RATE  = 0.*/NL%TREEFALL_DISTURBANCE_RATE  = 0.004/" ED2IN # turn on treefall
        sed -i "s/NL%INCLUDE_FIRE    = .*/NL%INCLUDE_FIRE    = $INC_FIRE/" ED2IN # turn on fire if run w/ fire on
        sed -i "s/NL%FIRE_PARAMETER  = .*/NL%FIRE_PARAMETER  = $FIRE_INT/" ED2IN # set fire intensity parameter
        sed -i "s/NL%SM_FIRE         = .*/NL%SM_FIRE         = $SM_FIRE/" ED2IN # set fire threshold parameter

		# spin spawn start changes -- 
		# Note: spins require a different first script because they won't have any 
		#       histo files to read
		cp ${setup_dir}spawn_startloops_spinstart.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops_spinstart.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" spawn_startloops_spinstart.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops_spinstart.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops_spinstart.sh # set the file path
	    sed -i "s,sub_post_process.sh,sub_post_process_spinfinish.sh,g" spawn_startloops_spinstart.sh # set the file path

		# spawn restarts changes
		cp ${setup_dir}spawn_startloops.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" spawn_startloops.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops.sh # set the file path
	    sed -i "s,sub_post_process.sh,sub_post_process_spinfinish.sh,g" spawn_startloops.sh # set the file path

		# adjust integration step changes
		cp ${setup_dir}adjust_integration_restart.sh .
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" adjust_integration_restart.sh 		
		sed -i "s/USER=.*/USER=${USER}/" adjust_integration_restart.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" adjust_integration_restart.sh 	
		sed -i "s/SITE=.*/SITE=${RUN}/" adjust_integration_restart.sh 		
	    sed -i "s,/dummy/path,${file_path},g" adjust_integration_restart.sh # set the file path
		
		# Clean up the spin initials since we don't need them anymore
		cp ${setup_dir}cleanup_spininit.sh .
	    sed -i "s,/DUMMY/PATH,${init_dir}${RUN}/,g" cleanup_spininit.sh # set the file path
		sed -i "s/RUN=.*/RUN=${RUN}/" cleanup_spininit.sh 		
	    sed -i "s/lastyear=.*/lastyear=${finalinit}/" cleanup_spininit.sh 		

	popd

	chmod -R a+rwx ${file_path}
	
done
