################################################################
# Example R-script to run the R-package SSM                   ##
# Title: Sewage treatment plant surface water model           ##
# Author 1: Martin Loos, mloos@envibee.ch [R-code]             #
# Author 2: Rebekka Gulde, rebekka.gulde@vsa.ch [application]  #
#                                                              #
#             https://github.com/blosloos/SSM/wiki             #


# installation: https://github.com/blosloos/SSM/wiki/Installation

library(devtools)
install_github("blosloos/SSM", ref = "main")
library(SSM)
################################################################

################################################################
# load input table and set path ################################

# path to sewage treatment plant (STP) input table as xlsx
xlsxFile_path <- "C:/VSA_PF_VT-Mikroverunreinigungen/SSM/20230619/input_table.xlsx"
# set name of xlsx-Sheet
sheet_name <- "model_inputs"

# load input table
input_table <- openxlsx:::read.xlsx(xlsxFile = xlsxFile_path, sheet = sheet_name, startRow = 3) # it starts to read at row 3, because there are descriptions in the first two rows of the example input_table
 
# set path to output folder
path_out <- "C:/VSA_PF_VT-Mikroverunreinigungen/SSM/20230619/outputs"


################################################################

################################################################
# The Script asks at several parameter for two values, a minimal and maximal one. It will calculate minimal and maximal results. 
# If you don't want that, put the same value twice. 

################################################################
# set the compound name of the compound you want to calculate
compound_name <- "Diclofenac"

# set scenario_name, which will be the name of the output files
scenario_name <- "DIC_2040_vgl_SFM2_v2"

# set compound load that enters the STP per inhabitant and day. E.g. from calculation of STP measurements or consumption data
compound_load_gramm_per_capita_and_day <- c(
	600 * 1E-6,	# minimum [g/inhabitant/d]
	600 * 1E-6	# maximum [g/inhabtant/d]
)

################################################################
# set compound elimination method. Needs to be "compound_specific" or "node_specific"
compound_elimination_method <- "compound_specific"

# if "compound_specific" is chosen  ############################
# you need to give a minimal and maximal elimination fraction of the compound [between 0 and 1] for each treatment 
compound_elimination_STP <- data.frame(	
  #if in the input table a STP is set to "FALSE" for "nitrification" then the following elimination fraction of COD_treatment is taken
	COD_treatment = c(0.0, 0.0), #biological treatment, which only removes chemical oxygen demand
	#if in the input table a STP is set to "TRUE" for "nitrification" then the following elimination fraction of nitrification is taken
	nitrification = c(0.0, 0.0), 
	
	#if in the input table a STP is set to "TRUE" for "denitrification" then the following elimination fraction of denitrification is taken 
	denitrification = c(0.0, 0.0), 
	#if in the input table a STP is set to "TRUE" for "P_elimination" then the following elimination fraction of P_elimination is taken 
	P_elimination = c(0.0, 0.0), # Phosphor elimination
	
		# additionally if a STP has an "type_advanced_treament" the respective elimination factor of the following is taken 
	GAC = c(0.85, 0.85),
	combi = c(0.99, 0.99),
	ozonation = c(0.99, 0.99),
	PAC = c(0.85, 0.85),
	undefined = c(0.99, 0.99)
)

# total elimination fraction = (COD_treatment OR nitrification) AND (denitrification OR none) AND (P_elimination or none) AND (GAC OR combi OR ozonation OR PAC or undefined or none) 
    # "AND" means mulitplication of the fraction
    # "none" means a elimination fraction of 1.0, i.e. no elimination. 


# if "compound_elimination_method" is set to "node_specific"  ##
# set names of columns of input table that contain the elimination fraction that shall be used for the respective STP
use_columns_STP_elimination_rate <- c("STP_elimination_min", "STP_elimination_max") # column with minimum and maximum values [values between 0 and 1]

################################################################
# If STP_reroute=TRUE, the wastewater of STPs that are set to "redirection" in the column "type_advanced_treatment", will be added to the STP, which ID is given under "redirecting_STP_target_STP_ID" in the input table  
# this applies only for STPs, for which the "starting_year_advanced_treatment" is before or the same as "STP_scenario_year", if "STP_filter_steps"=TRUE
STP_reroute <- TRUE						# TRUE or FALSE 

################################################################
# If "STP_filter_steps" is set to TRUE the script will consider all advanced treatment/redirection (as given in column "type_advanced_treatment"),
# which are set in column "starting_year_advanced_treatment" before or in the same year than "STP_scenario_year". 
STP_filter_steps = TRUE # TURE or FALSE 
STP_scenario_year <- 2040 # yyyy

################################################################
# If "TRUE", the load of all STPs upstream of lakes will be reduced by the factor that is given in columns "lake_elimination_min" and	"lake_elimination_max" in the input table
with_lake_elimination <- TRUE				# TRUE or FALSE

################################################################
# set names of columns of input table that contain the river discharge 
use_columns_local_river_discharge <- c("Q347_L_s_min", "Q347_L_s_max") # column with minimum and maximum values in L/s
# the first of these two columns is used to calculate the fraction_STP_discharge_of_river

################################################################
# the total STP discharge will be calculated by the "inhabitants" of the STP and "STP_discharge_per_capita"
# The "STP_discharge_per_capita" is the volume of wastewater per inhabitant 
STP_discharge_per_capita <- 375 # [L/inhabitant/day]

################################################################
# If "add_absolute_load" is set to TRUE, the load of the respective STPs (or other nodes) as given in the column "additional_absolute_load" are added 
# This can be used, if industrial, agricultural or other input should be added
add_absolute_load <- TRUE					# TRUE or FALSE


################################################################
#set "all_columns" to "TRUE", if you want all columns of the input-table in the output-table. otherwise only the results are in the output-table
all_columns <- TRUE # TRUE or FALSE 

################################################################
################################################################
# run calculations and save as csv under output path############

if (all_columns==TRUE){add_columns_from_input_table <- colnames(input_table[-1])}else{add_columns_from_input_table<-c("name")}

wrap_table(
	input_table = input_table,									
	STP_scenario_year = STP_scenario_year,
	STP_filter_steps = STP_filter_steps,
	STP_reroute = STP_reroute,									
	STP_discharge_per_capita = STP_discharge_per_capita,						
	compound_name = compound_name,
	compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day,
	compound_elimination_method = compound_elimination_method,	
	compound_elimination_STP = compound_elimination_STP,						
	with_lake_elimination = with_lake_elimination,
	add_absolute_load = add_absolute_load, 
	use_columns_local_river_discharge = use_columns_local_river_discharge,
	use_STP_elimination_rate = use_columns_STP_elimination_rate,
	add_columns_from_input_table = add_columns_from_input_table,
	scenario_name = scenario_name,
	path_out = path_out,									
	overwrite = TRUE, 
	write_csv = TRUE,									
	use_sep_csv = ";"
)

################################################################






































