#!bin/bash
# This file starts the next runs from the PalEON Regional ED Runs
# Christy Rollinson, crollinson@gmail.com

# Things to specify
# n          = Number of RUNs to start
# ED2IN_Base = template ED2IN to be modified
# file.dir   = spininit directory; used to find what RUNs have been done
# soil.path  = path of percent clay and percent sand to query for
#              SLXCLAY & SLXSAND, respectively
# grid.order = .csv file with the order RUNs should be run in to determine 
#              what RUNs should be done next


# Order of Operations
# 1) Sync file with order RUNs & status/location 
# 2) Add file directories for any RUNs that are remote so we don't repeat them
# 3) loop through the next n runs and adjust base ED2IN for specific characters
#    Things to be Modified per RUN:
#     -  NL%POI_LAT  =  
#     -  NL%POI_LON  = 
#     -  NL%FFILOUT = '~/ED_PalEON/MIP2_Region/1_spin_initial/phase2_spininit.v1/XXXXX/analy/XXXXX'
#     -  NL%SFILOUT = '~/ED_PalEON/MIP2_Region/1_spin_initial/phase2_spininit.v1/XXXXX/histo/XXXXX'
#     -  NL%SFILIN  = '~/ED_PalEON/MIP2_Region/1_spin_initial/phase2_spininit.v1/XXXXX/histo/XXXXX'
#     -  NL%SLXCLAY = 
#     -  NL%SLXSAND = 


## Load the necessary hdf5 library
# module load hdf5/1.6.10
# module load nco/4.3.4

# Define constants & file paths for the scripts
BU_base_spin=/projectnb/dietzelab/paleon/ED_runs/MIP2_Region # The base original file paths in all of my scripts
BU_base_EDI=/projectnb/dietzelab/EDI/ # The location of basic ED Inputs on the BU server

file_base=~/URF2018-Butkiewicz/ED_Workflow # whatever you want the base output file path to be
EDI_base=/home/models/ED_inputs/ # The location of basic ED Inputs for you
met_base=/home/models/ED_MET/GLSP.v1

ed_exec=/home/models/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
file_dir=${file_base}/1_spin_initial/URF2018_spininit.v3/ # Where everything will go
setup_dir=${file_base}/0_setup/ # Where some constant setup files are
RUN_file=${setup_dir}/ExperimentalDesign.csv # # Path to list of ED RUNs w/ status

# # Lets double check and make sure the order status file is up to date
# # Note: need to make sure you don't have to enter a password for this to work right
# git fetch --all
# git checkout origin/master -- $RUN_file

finalyear=2801
finalfull=2800
n=1

# Make sure the file paths on the Met Header have been updated for the current file structure
sed -i "s,$BU_base_spin,$file_base,g" ${file_base}/0_setup/PL_MET_HEADER

# Making the file directory if it doesn't already exist
mkdir -p $file_dir

# Extract the file names of RUNs that haven't been started yet
runs_all=($(awk -F ',' 'NR>1 && $14=="" {print $2}' ${RUN_file}))
lat_all=($(awk -F ',' 'NR>1 && $14=="" {print $4}' ${RUN_file}))
lon_all=($(awk -F ',' 'NR>1 && $14=="" {print $3}' ${RUN_file}))

# These will need to get updated with the proper column numbers
met_all=($(awk -F ',' 'NR>1 && $14=="" {print $5}' ${RUN_file}))
sand_all=($(awk -F ',' 'NR>1 && $14=="" {print $6}' ${RUN_file}))
clay_all=($(awk -F ',' 'NR>1 && $14=="" {print $7}' ${RUN_file}))
inc_fire_all=($(awk -F ',' 'NR>1 && $14=="" {print $9}' ${RUN_file})) # INCLUDE_FIRE
sm_fire_all=($(awk -F ',' 'NR>1 && $14=="" {print $11}' ${RUN_file})) # SM_FIRE
fire_int_all=($(awk -F ',' 'NR>1 && $14=="" {print $12}' ${RUN_file})) # FIRE_INTENSITY

# Get the list of what grid runs have already finished spinups
pushd $file_dir
	file_done=(C*)
popd
file_done=(${file_done[@]/"C*"/})

# Because we want to preserve the order of runs, I can't find away around doing a loop
# - This is slower than other options, but makes sure we still do our controls first
# - DO NOT imitate this with a large array
runs=()
lat=()
lon=()
met=()
sand=()
clay=()
inc_fire=()
sm_fire=()
fire_int=()

for((i=0;i<${#runs_all[@]};i++)); do 
	RUN=${runs_all[i]}
    TEST=( ${file_done[@]/$RUN/} ) # Remove element from array

	# If the length of TEST is still the same, we haven't done it yet
    if [[ ${#TEST[@]} == ${#file_done[@]} ]]; then
		runs+=("$RUN")
		lat+=("${lat_all[i]}")
		lon+=("${lon_all[i]}")
		met+=("${met_all[i]}")
		sand+=("${sand_all[i]}")
		clay+=("${clay_all[i]}")
		inc_fire+=("${inc_fire_all[i]}")
		sm_fire+=("${sm_fire_all[i]}")
		fire_int+=("${fire_int_all[i]}")
	fi    

done



n=$(($n<${#runs[@]}?$n:${#runs[@]}))

# for FILE in $(seq 0 (($n-1)))
for ((FILE=0; FILE<$n; FILE++)) # This is a way of doing it so that we don't have to modify N
do
	# RUN Name and Lat/Lon
	RUN=${runs[FILE]}
	LAT=${lat[FILE]}
	LON=${lon[FILE]}	
	MET=${met[FILE]}
	SAND=${sand[FILE]}
	CLAY=${clay[FILE]}
	INC_FIRE=${include_fire[FILE]}
	SM_FIRE=${sm_fire[FILE]}
	FIRE_INT=${fire_intensity[FILE]}
	
	echo $RUN

	
	# File Paths
    new_analy="'${file_dir}${RUN}/analy/${RUN}'"
    new_histo="'${file_dir}${RUN}/histo/${RUN}'"
    newbase=${file_dir}/$RUN
	oldname=TESTinit
	met_path=${met_base}/${MET}


	file_path=${file_dir}/${RUN}/

	mkdir -p ${file_path} 
	
	pushd ${file_path}
		# Creating the default file structure and copying over the base files to be modified
		mkdir -p histo analy
		ln -s $ed_exec
		cp ../../ED2IN_SpinInit_Base ED2IN
		cp ${setup_dir}PFTParams_URF2018.xml .

		# ED2IN Changes	    
		sed -i "s,/dummy/path,${file_dir},g" ED2IN # set the file path
		sed -i "s,/met/path,${met_path},g" ED2IN # set the file path
	    sed -i "s,TEST,${RUN},g" ED2IN #change site ID

        sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = $finalyear/" ED2IN # Set last year
        sed -i "s/POI_LAT  =.*/POI_LAT  = $LAT/" ED2IN # set RUN latitude
        sed -i "s/POI_LON  =.*/POI_LON  = $LON/" ED2IN # set RUN longitude
        sed -i "s/SLXCLAY =.*/SLXCLAY = $CLAY/" ED2IN # set fraction soil clay
        sed -i "s/SLXSAND =.*/SLXSAND = $SAND/" ED2IN # set fraction soil sand
   		sed -i "s/FIRE_PARAMETER  =.*/FIRE_PARAMETER  = $FIRE_INT/" ED2IN # Set the fire parameter for later

		# spin spawn start changes -- 
		# Note: spins require a different first script because they won't have any 
		#       histo files to read
		cp ${setup_dir}spawn_startloops_spinstart.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops_spinstart.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" spawn_startloops_spinstart.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops_spinstart.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops_spinstart.sh # set the file path

		# spawn restarts changes
		cp ${setup_dir}spawn_startloops.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" spawn_startloops.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops.sh # set the file path

		# adjust integration step changes
		cp ${setup_dir}adjust_integration_restart.sh .
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" adjust_integration_restart.sh 		
		sed -i "s/USER=.*/USER=${USER}/" adjust_integration_restart.sh
		sed -i "s/RUN=.*/RUN=${RUN}/" adjust_integration_restart.sh 	
		sed -i "s/SITE=.*/SITE=${RUN}/" adjust_integration_restart.sh 		
	    sed -i "s,/dummy/path,${file_path},g" adjust_integration_restart.sh # set the file path
		
	popd	

	chmod -R a+rwx ${file_path}

done

# git stash # stash the pulled file so we don't get confilcts

