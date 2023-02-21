################################################################
# -> Paket ggfs. installieren (R hierfür als Admin ausführen) ##
# (library devtools und Rtools müssen vorinstalliert sein) #####

library(devtools)
install_github("blosloos/SSM", ref = "main")

################################################################

################################################################
# -> Paket & ARA-Tabelle laden & formatieren, Output-Pfad ######

library(SSM)

# Pfad zu ARA-Tabelle setzen & xlsx-Sheet benennen:
xlsxFile_path <- "C:/Projects/VSA/round_fin/model_inputs.xlsx"
sheet_name <- "model_inputs"

input_table <- openxlsx:::read.xlsx(xlsxFile = xlsxFile_path, sheet = sheet_name, startRow = 3)
 
# Output-Pfad setzen:
path_out <- "C:/Projects/VSA/round_fin/example_outputs"


################################################################

################################################################
# Substanz- & weitere Angaben ##################################
# -> können für Fehlerberechnung teilweise zwei Werte enthalten 

compound_name <- "Diclofenac"
compound_load_gramm_per_capita_and_day <- c(
	100 * 1E-6,	# kleinerer Wert
	500 * 1E-6	# grösserer Wert
)

# jeweils kleinere, grössere Elimination (oder gleiche Werte, z.B. für P_elimination; oder 0, falls ohne Einfluss)
# muss auch bei compound_elimination_method <- "STP individual" ausgefüllt werden, wird dort aber ignoriert # <-CHANGE!!!
compound_elimination_STP <- data.frame(	
	COD_treatment = c(0.4, 0.5),
	nitrification = c(0.3, 0.6),
	denitrification = c(0.15, 0.2),
	P_elimination = c(0.1, 0.2),
	GAC = c(0, 0.15),
	combi = c(0, 0.05),
	ozonation = c(0.4, 0.7),
	PAC = c(0, 0.3),
	undefined = c(0, 0.12)
)

compound_elimination_method <- "compound_specific" # "compound_specific", "node_specific"


# Welche Spalten beinhalten den Abfluss, kleinerer und grösserer Wert? 
use_columns_local_discharge <- c("Q347_L_s_min", "Q347_L_s_max") 
use_columns_local_discharge_for_fractions <- "Q347_L_s_min"

# Welche Spalten enthalten ARA-spezifische Eliminationsraten, kleinerer und grösserer Wert? 
use_columns_STP_elimination_rate <- c("STP_elimination_min", "STP_elimination_max")

STP_discharge_per_capita <- 400
STP_scenario_year <- 2030
STP_reroute <- FALSE						# Umleitungen für STP_scenario_year berücksichtigen?

with_lake_elimination <- TRUE				# Elimination rates now in STP_table, columns lake_elimination_min und lake_elimination_max

add_absolute_load <- TRUE					# additiert absolute Fracht aus Spalte additional_absolut_load in STP_table -> zB inputs aus Landwirtschaft

################################################################

################################################################
# -> Berechnen, Output-Tabelle in path_out_xlsx ################



wrap_table(

	model_input_table = model_input_table,									
	STP_scenario_year = as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1]),
	STP_reroute = TRUE,									
	STP_filter_steps = TRUE,							
	STP_discharge_per_capita = 400,						
	compound_name = compound_name,
	scenario_name = compound_name,
	compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day,				
	compound_elimination_STP = compound_elimination_STP,					
	compound_elimination_method = "micropollutants",	
	with_lake_elimination = TRUE,
	add_absolute_load = TRUE,
	use_columns_local_discharge = c("Q347_L_s_min", "Q347_L_s_max"),
	use_STP_elimination_rate = use_columns_STP_elimination_rate,
	add_columns_from_model_input_table = c("ID_next", "X_position", "Y_position"),
	path_out = path_out,									
	overwrite = TRUE,
	write_csv = TRUE,									
	use_sep_csv = ","
	
)



	model_input_table = model_input_table									
	STP_scenario_year = as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1])
	STP_reroute = FALSE
	STP_filter_steps = FALSE
	STP_discharge_per_capita = 400
	compound_name = compound_name
	scenario_name = compound_name
	compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day
	compound_elimination_STP = compound_elimination_STP	
	compound_elimination_method = "micropollutants"
	with_lake_elimination = FALSE
	add_absolute_load = FALSE
	use_columns_local_discharge = c("Q347_L_s_min", "Q347_L_s_max")
	use_STP_elimination_rate = use_columns_STP_elimination_rate
	add_columns_from_model_input_table = c("ID_next", "X_position", "Y_position")
	path_out = path_out
	overwrite = TRUE
	write_csv = TRUE
	use_sep_csv = " "







# not used in script




	STP_id = NULL
	STP_id_next = NULL
	STP_amount_inhabitants = NULL
	STP_local_discharge_river = NULL					# discharge in river at STP
	STP_treatment_steps = NULL
	STP_amount_people_local = NULL						# amount of people at STP
	
	
	compound_load_total = FALSE 						# [kg / a]


	compound_excreted = 1								# fraction excreted and discharged, set to 1 to ignore
	
	STP_amount_hospital_beds

	STP_fraction_hospital

hospital_beds_total

	path_out = FALSE,									# if FALSE, return data.frame
	overwrite = TRUE,
	write_csv = TRUE,									# else, exports an excel file

	# deprecated:
	compound_elimination_STP$COD_treatment

################################################################






































