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
xlsxFile_path <- "E:/VSA/new_inputs/ARA_Inputdaten_v6.xlsx"
sheet_name <- "ARA_Inputdaten_v1"

STP_table <- openxlsx:::read.xlsx(xlsxFile = xlsxFile_path, sheet = sheet_name)
 
# Output-Pfad setzen:
path_out <- "E:/VSA/new_outputs"


################################################################

################################################################
# Substanz- & weitere Angaben ##################################
# -> können für Fehlerberechnung teilweise zwei Werte enthalten 

compound_name <- "Diclofenac"
compound_load_gramm_per_capita_and_day <- c(
	500 * 1E-6,	# grösserer Wert
	100 * 1E-6	# kleinerer Wert
)

# jeweils kleinere, grössere Elimination (oder gleiche Werte, z.B. für P_elimination; oder 0, falls ohne Einfluss)
# muss auch bei compound_elimination_method <- "STP individual" ausgefüllt werden, wird dort aber ignoriert # <-CHANGE!!!
compound_elimination_STP <- data.frame(	
	CSB_Abbau = c(0.4, 0.5),
	nitrification = c(0.3, 0.6),
	denitrification = c(0.15, 0.2),
	elevated_denitrification = c(0.15, 0.2),	
	P_elimination = c(0, 0),
	GAK = c(0, 0.15),
	Kombi = c(0, 0.05),
	Ozonung = c(0.4, 0.7),
	PAK = c(0, 0.3),
	Ausbau = c(0, 0.12)
)

compound_elimination_method <- "STP individual" # "micropollutants", "STP individual"

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

wrap_vsa(

	STP_table = STP_table,			
	STP_discharge_per_capita = STP_discharge_per_capita,
	STP_scenario_year = STP_scenario_year,
	STP_reroute = STP_reroute,
	
	with_lake_elimination = with_lake_elimination,
	add_absolute_load = add_absolute_load,
	
	compound_name = compound_name,
	compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day,	
	compound_elimination_STP = compound_elimination_STP,
	compound_elimination_method = compound_elimination_method,
	
	use_columns_local_discharge = use_columns_local_discharge,
	use_columns_local_discharge_for_fractions = use_columns_local_discharge_for_fractions,
	use_STP_elimination_rate = use_columns_STP_elimination_rate,
	add_columns_from_STP_table = c("STP_next", "X_position", "Y_position"),
	
	path_out = path_out,
	use_sep_csv = ","
	
)


# not used in script



	STP_filter_steps = TRUE							#	 Filter STP treatment steps until a given STP_scenario_year
	STP_id = NULL
	STP_id_next = NULL
	STP_amount_inhabitants = NULL
	STP_local_discharge_river = NULL					# discharge in river at STP
	STP_treatment_steps = NULL
	STP_amount_people_local = NULL						# amount of people at STP
	
	
	compound_load_total = FALSE 						# [kg / a]

	compound_load_per_hospital_bed_and_day = 0			# [g / E d], set to FALSE to ignore

	compound_excreted = 1								# fraction excreted and discharged, set to 1 to ignore
	



	path_out = FALSE,									# if FALSE, return data.frame
	overwrite = TRUE,
	write_csv = TRUE,									# else, exports an excel file


################################################################






































