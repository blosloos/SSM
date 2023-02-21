

package.skeleton(
	name = "SSM", 
	environment = .GlobalEnv,
	path = "C:/PART_4/MS/R_packages/SSM", 
	force = FALSE, code_files = character(), encoding = "unknown"
)




library(devtools)

load_all("C:/PART_4/MS/R_packages/SSM")

document(pkg = "C:/PART_4/MS/R_packages/SSM", roclets = NULL, quiet = FALSE)




ID <- c(1, 2, 3, 4, 5)	# nodes: STP, STP, STP, lake, STP
ID_next <- c(4, 3, 4, 5, NA)	# node with ID = 5 has no downstream node (hence ID_next = NA)
inhabitants <- c(403, 150, 324, NA, 172)	# all excertion is cleaned in STPs, none hoes into the lake
compound_load_gramm_per_capita_and_day <- 100 * 1E-6

compound_elimination_STP <- data.frame(
	COD_treatment = 0.5, nitrification = 0.6,
	denitrification = 0.2, P_elimination = 0.2,
	GAC = 0, combi = 0.05, ozonation = 0.7,
	PAC = 0.3, undefined = 0.12
)

STP_treatment_steps <- cbind(	# define presence of treatment steps
	"nitrification" = c("TRUE", "TRUE", "FALSE", "none", "TRUE"),
	"denitrification" = c("TRUE", "FALSE", "FALSE", "none", "TRUE"), 
	"P_elimination" = c("TRUE", "TRUE", "TRUE", "none", "TRUE"),
	"type_advanced_treatment" = c("GAC", "", "", "", "ozonation")
)
rownames(STP_treatment_steps) <- ID

lake_eliminination_rates <- c(NA, NA, NA, 0.1, NA)	# set only for lake, NA otherwise
 
calc_load(
	ID, ID_next, inhabitants,	
	compound_load_gramm_per_capita_and_day,		
	compound_elimination_method = "compound_specific",
	compound_elimination_STP,
	STP_treatment_steps,
	with_lake_elimination = TRUE, 
	lake_eliminination_rates
)

	compound_elimination_method = "compound_specific"
	with_lake_elimination = TRUE
	add_absolute_load = FALSE
	absolute_loads = FALSE
	return_data = "loads"




unload(pkg = "C:/PART_4/MS/R_packages/SSM")
clean_dll(pkg = "C:/PART_4/MS/R_packages/SSM")

compile_dll(pkg = "C:/PART_4/MS/R_packages/SSM", quiet = FALSE)
load_all(pkg = "C:/PART_4/MS/R_packages/SSM")

#
unload(pkg = "C:/PART_4/MS/R_packages/SSM")
clean_dll(pkg = "C:/PART_4/MS/R_packages/SSM")

compile_dll(pkg = "C:/PART_4/MS/R_packages/SSM", quiet = FALSE)
load_all(pkg = "C:/PART_4/MS/R_packages/SSM")



library(devtools)

install_github("blosloos/SSM")


library(devtools)

install(pkg = "C:/PART_4/MS/R_packages/SSM")


#################################################################################
# WORK on ARA table 


ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input.csv", header = TRUE, sep = ",")

# 
ARA$X_position

ARA$X_Koordinate[is.na(ARA$X_position)]



sum(is.na(ARA$X_Koordinate))





plot(ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & !is.na(ARA$X_position)], ARA$X_position[!is.na(ARA$X_Koordinate) & !is.na(ARA$X_position)])

ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & !is.na(ARA$X_position)] - ARA$X_position[!is.na(ARA$X_Koordinate) & !is.na(ARA$X_position)]


plot(ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & !is.na(ARA$Y_position)], ARA$Y_position[!is.na(ARA$Y_Koordinate) & !is.na(ARA$Y_position)])

ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & !is.na(ARA$Y_position)] - ARA$Y_position[!is.na(ARA$Y_Koordinate) & !is.na(ARA$Y_position)]


ARA$X_position[!is.na(ARA$X_Koordinate) & is.na(ARA$X_position)] <- ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & is.na(ARA$X_position)] + 2000000

ARA$Y_position[!is.na(ARA$Y_Koordinate) & is.na(ARA$Y_position)] <- ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & is.na(ARA$Y_position)] + 1000000


ARA$X_position[is.na(ARA$X_position)]
ARA$Y_position[is.na(ARA$Y_position)]




Vorfluter_X_position <- ARA$Vorfluter_X_Koordinate + 2000000
Vorfluter_Y_position <- ARA$Vorfluter_Y_Koordinate + 1000000

ARA <- cbind(ARA, Vorfluter_X_position, Vorfluter_Y_position)



write.csv(ARA, file = "D:/VSA/new_inputs/ARA_input_corrected.csv")

ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",")

ARA$STP_next %in% ARA[, 2]


ARA_missing_ARANEXT <- ARA[!is.na(ARA$STP_next),][!(ARA$STP_next[!is.na(ARA$STP_next)] %in% ARA[, 2]),]





ARA_missing_ARANEXT$STP_next %in% ARA[, 5] 





write.csv(ARA_missing_ARANEXT, file = "D:/VSA/new_inputs/ARA_missing_ARANEXT.csv")


#################################################################################
# run

ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)








