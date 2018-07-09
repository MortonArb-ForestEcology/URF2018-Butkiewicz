##########################################################
# Investigating the Output files of our initial Spin-Ups # 
##########################################################

# The path scripts will	be run so that:                	
###   .ds = dry, sandy
###   .dc = dry, clay-y
###   .ws = wet, sandy
###   .wc = wet, clay-y
path.ds <- ("../ED_Workflow/1_spin_initial/extracted_ouput/CD-SS-FN-TN-IN")
files.ds <- dir(path.ds)
class(files.ds)
length(files.ds)
print(files.ds)