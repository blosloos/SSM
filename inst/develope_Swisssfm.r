




package.skeleton(
	name = "SSMdata",
	environment = .GlobalEnv,
	path = "C:/PART_4/MS/R_packages/", 
	force = FALSE, code_files = character(), encoding = "unknown"
)




library(devtools)

load_all("C:/PART_4/MS/R_packages/SSM")


document(pkg = "C:/PART_4/MS/R_packages/SSM", roclets = NULL, quiet = FALSE)



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








