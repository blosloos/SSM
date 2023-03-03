#' Calculate ranges of compound loads and concentrations in a river network under different scenarios of sewage treatment.
#'
#'
#' @inheritParams calc_load
#' @param input_table Dataframe containing various information on sewage treatment plants and lakes, cp. example data [input_table]
#' @param STP_scenario_year 4-digit integer value for the scenario year. Defaults to the current year.
#' @param STP_reroute Logical. Reroute STPs up to STP_scenario_year? Defaults to TRUE.
#' @param STP_filter_steps Logical. Ignore STP treatment steps from a given `STP_scenario_year` onwards (cp. details)? Defaults to TRUE.
#' @param STP_discharge_per_capita Single numeric value. Discharge per person and day `[l / d]`. 
#' @param compound_name Character string, name of the compound under consideration. Will be written into the exported Excel file, if such is used instead of csv. 
#' @param compound_load_gramm_per_capita_and_day Vector of tow numeric value. Minimum and maximum amounts of the compound excreted per person and day `[g / d]`.
#'
#' @param compound_elimination_STP Required for compound_elimination_method `"compound_specific"`. 
#' Dataframe with two rows for minium and maximum elimination fractions `[0, 1]` for the following STP treatment steps aka column names:
#'* COD_treatment
#'* nitrification
#'* denitrification
#'* P_elimination
#'* GAC
#'* combi
#'* ozonation
#'* PAC
#'* undefined: placeholder for yet unspecified but scheduled advanced treatment, e.g., a mean elimination fraction for GAC, combi, ozonation and PAC.
#'
#' @param use_columns_local_river_discharge Character vector with two column names from `input_table` for minimum and maximum river discharge `[l / s]` just downstream of an STP or lake. 
#' The defaults is `c("Q347_L_s_min", "Q347_L_s_max")`. 
#' @param use_STP_elimination_rate Character vector with two column names from `input_table` for minimum and maximum STP-specific elimination rates, 
#' required for compound_elimination_method `"node_specific"`. Defaults to `c("STP_elimination_min", "STP_elimination_max")`. 
#' @param add_columns_from_input_table Character vector with names of columns from `input_table` to be attached and exported with the results table.  
#' @param scenario_name Character string defaulting to `compound_name`. Name of the exported csv or Excel file path_out is not NULL. 
#' @param path_out NULL (default) or string containing folder path for writing csv or Excel (not containing the file name, cp. `scenario_name`). 
#' @param overwrite Logical, used if path_out is not NULL. Overwrite any existing file? Defaults to TRUE.
#' @param write_csv Logical, used if path_out is not NULL. Defaults to TRUE to write both exported files as csv, else exports one of these as Excel file (cp. return value section). 
#' @param use_sep_csv String for csv sperataror, used if path_out is not NULL. 
#'
#'
#' @description 
#' Wrapper function on [calc_load()] to calculate minimum and maximum loads and concentrations of a compound in a river network from an input table 
#' containing information on sewage treatment plants (STPs) and lake nodes. `STP_scenario_year` allows simulations for different stages of advanced treatment
#' or a rerouting of STPs.
#'
#'
#' @details Check function [calc_load()] for the underlying approach on calculating loads from human compound inputs and their elimination in STPs (cp. argument `compound_elimination_method`) 
#' and lakes (cp. `with_lake_elimination`).
#' Using [table_input], this wrapper extends this approach by estimating minimum and maximum loads and their aquatic concentrations.
#' By using an `STP_scenario_year`, `STP_filter_steps = TRUE` and column `starting_year_advanced_treatment` in [input_table] (and possibly in combination with `STP_reroute`) 
#' data for different points in time can be simulated. That is, treatment steps that are built/operated at an STP after the used `STP_scenario_year` are ignored.
#'
#'
#' @note In contrast to column `type_advanced_treatment` of argument `STP_treatment_steps` in function [calc_load()], 
#' the same column in `input_table` may also contain the entry `"redirection"`, apart from `"GAC"`, `"combi"`, `"ozonation"`, `"PAC"` or `"undefined"`.
#' It redirects the influent from one STP to another `redirecting_STP_target_STP_ID` (another column in [input_table]) when `STP_reroute` is set to TRUE, in combination
#' with argument `STP_scenario_year` and `starting_year_advanced_treatment` (yet another column in [input_table]) .
#'
#'
#' @returns This function either returns a dataframe (if `path_out` is not specified) or saves two file named `scenario_name_topo_matrix` and
#' `scenario_name` (or, if not provided, `compound_name`) into folder `path_out`. 
#' The first is always a csv format and contains the topology (i.e. routing) matrix used for the calculations. 
#' The second file is either a csv of Excel format (depending on `write_csv`) with the following result columns:
#' 
#'* `ID`: Node IDs of STPs and lakes
#'* `inhabitants_cumulated`: cumulated number of inhabitants just downstream of each STP or lake (these numbers are not affected by elimination).
#'* `STP_local_discharge_L_s`: product of inhabitants and STP_discharge_per_capita from each STP `[l / s]`
#'* `STP_cumulated_discharge_L_s`: cumulated product of inhabitants and STP_discharge_per_capita just downstream each STP or lake node `[l / s]`
#'* `node_count_cumulated`: cumulated number of nodes upstream of a each STP or lake node
#'* `Fraction_STP_discharge_of_river_local`: STP_local_discharge_L_s divided by the local river discharge (i.e., first column in argument `use_columns_local_river_discharge`)
#'* `Fraction_STP_discharge_of_river_cumulated`: STP_cumulated_discharge_L_s divided by the local river discharge (i.e., first column in argument `use_columns_local_river_discharge`)
#'* `Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated`: local discharge (i.e., first column in argument `use_columns_local_river_discharge`) divided by 
#' the sewage discharge from STPs without advanced treatments.
#'
#'* `Fraction_STP_discharge_of_river_local_includingSTPdischarge`: STP_local_discharge_L_s divided by the sum of local river discharge (i.e., first column in argument 
#' `use_columns_local_river_discharge`) and `STP_local_discharge_L_s`
#'
#'* `Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge`: STP_cumulated_discharge_L_s divided by the sum of local river discharge (i.e., first column in argument 
#' `use_columns_local_river_discharge`) and `STP_local_discharge_L_s`
#'
#'* `Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated_includingSTPdischarge`: STP_cumulated_discharge_L_s from STPs without advanced treatment divided by
#' the sum of local river discharge (i.e., first column in argument `use_columns_local_river_discharge`) and `STP_local_discharge_L_s`
#'
#'* `Fraction_of_wastewater_only_C_removal`: fraction of cumulated STP discharge from STPs without nitrification, denitrification and any advanced treatment step, just downstream of each STP or lake.
#'* `Fraction_of_wastewater_nitrification`: fraction of cumulated STP discharge from STPs with nitrification, but without denitrification and any advanced treatment step, just downstream of each STP or lake. 
#'* `Fraction_of_wastewater_denitrification`: fraction of cumulated STP discharge from STPs with nitrification and denitrification but without any advanced treatment step, just downstream of each STP or lake.  
#'* `Fraction_of_wastewater_advanced_treatment`: fraction of cumulated STP discharge from STPs with an advanced treatment step, just downstream of each STP or lake.  
#'
#'* columns `input_load_local_g_d_max` and `input_load_local_g_d_min`: maximum and minimum compound amount released into each STP `[g / d]`, i.e., inhabitants * compound_load_gramm_per_capita_and_day 
#'
#'* columns `load_local_g_d_max` and `load_local_g_d_min`: maximum and minimum compound amount discharged from each STP after elimination `[g / d]`
#'
#'* columns `load_cumulated_g_d_max` and `load_cumulated_g_d_min`: maximum and minimum cumulated compound amount just downstream of each STP or lake `[g / d]`
#'
#'* columns `conc_local_ug_L_max` and `conc_local_ug_L_min`: `load_local_g_d_max` and `load_local_g_d_min` divided by local river discharge (i.e., relevant min/max columns in argument `use_columns_local_river_discharge`)
#'
#'* columns `conc_cumulated_ug_L_max` and `conc_cumulated_ug_L_min`: `load_cumulated_g_d_max` and `load_cumulated_g_d_min` divided by local river discharge (i.e., relevant min/max columns in argument `use_columns_local_river_discharge`)
#'
#' The first rows in the csv or the Excel spreadsheet also contain the parameters and the `compound_name` used.
#'
#' @seealso [calc_load()], [input_table]
#'
#' @examples 
#'
#' compound_load_gramm_per_capita_and_day <- c(100 * 1E-6,	500 * 1E-6)
#'
#' compound_elimination_STP <- data.frame(	
#'		COD_treatment = c(0.4, 0.5),
#'		nitrification = c(0.3, 0.6),
#'		denitrification = c(0.15, 0.2),
#'		P_elimination = c(0.1, 0.2),
#'		GAC = c(0, 0.15),
#'		combi = c(0, 0.05),
#'		ozonation = c(0.4, 0.7),
#'		PAC = c(0, 0.3),
#'		undefined = c(0, 0.12)
#' )
#'
#' results <- wrap_table(
#'		input_table = input_table,
#'		STP_scenario_year = 2030,
#'		STP_reroute = TRUE,
#'		STP_filter_steps = TRUE,
#'		STP_discharge_per_capita = 375,
#'		compound_name = "Diclofenac",
#'		compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day,
#'		compound_elimination_method = "compound_specific",
#'		compound_elimination_STP = compound_elimination_STP,
#'		with_lake_elimination = TRUE,
#'		add_absolute_load = TRUE,
#'		use_columns_local_river_discharge = c("Q347_L_s_min", "Q347_L_s_max"),
#'		use_STP_elimination_rate = c("STP_elimination_min", "STP_elimination_max"),
#'		add_columns_from_input_table = c("ID_next", "X_position", "Y_position"),
#'		scenario_name = compound_name,	
#'		path_out = NULL
#')
#'
#'
#' # Above [input_table] is lazy-loaded for this package.
#' # In addition, an Excel version model_inputs.xlsx of [input_table] exists in the extdata folder of this package, to be found under:
#' list.files(system.file("extdata", package = "SSM"))
#' # The first two rows contain comments, and must be skipped when loading via:
#' xlsxFile_path <- file.path( system.file("extdata", package = "SSM"), "model_inputs.xlsx")
#' input_table <- openxlsx:::read.xlsx(xlsxFile = xlsxFile_path, sheet = "model_inputs", startRow = 3)
#'
#'



wrap_table <- function(
	input_table,
	STP_scenario_year = as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1]),
	STP_reroute = TRUE,
	STP_filter_steps = TRUE,
	STP_discharge_per_capita = 375,
	compound_name = "compound_not_specified",
	compound_load_gramm_per_capita_and_day,
	compound_elimination_method = NULL,
	compound_elimination_STP = NULL,
	with_lake_elimination = FALSE,
	add_absolute_load = FALSE,
	use_columns_local_river_discharge = c("Q347_L_s_min", "Q347_L_s_max"),
	use_STP_elimination_rate = c("STP_elimination_min", "STP_elimination_max"),
	add_columns_from_input_table = NULL, #c("ID_next", "X_position", "Y_position"),
	scenario_name = compound_name,	
	path_out = NULL,
	overwrite = TRUE,
	write_csv = TRUE,
	use_sep_csv = ","
){

	###############################################
	# checks / extract input_table columns
	if(!identical(
		length(compound_load_gramm_per_capita_and_day),
		length(use_columns_local_river_discharge),
		nrow(compound_elimination_STP)
	)) stop("Problem in wrap_table: compound_load_gramm_per_capita_and_day, use_columns_local_river_discharge and the number of rows in compound_elimination_STP must be of equal length")	
	if(length(compound_load_gramm_per_capita_and_day) != 2) stop("Problem in wrap_table: compound_load_gramm_per_capita_and_day, use_columns_local_river_discharge and the number of rows in compound_elimination_STP must have two entries")
	if(nrow(compound_elimination_STP) != 2) stop("Problem in wrap_table: compound_elimination_STP must have two rows with min/max values")
	if(length(compound_load_gramm_per_capita_and_day) == 2){
		compound_load_gramm_per_capita_and_day <- sort(compound_load_gramm_per_capita_and_day,  decreasing = TRUE)
		if(any(compound_elimination_STP[1, ] > compound_elimination_STP[2, ])) stop("Problem in wrap_table: compound_elimination_STP set incorrectly for range calculation")
		if(any(input_table[, use_columns_local_river_discharge[1]] > input_table[, use_columns_local_river_discharge[2]], na.rm = TRUE)) stop("Problem in wrap_table: use_columns_local_river_discharge set incorrectly for range calculation")	
	}
	use_columns_local_river_discharge_for_fractions <- use_columns_local_river_discharge[1]
	if((compound_elimination_method == "node_specific") & (use_STP_elimination_rate[1] == FALSE)) stop("Problem in wrap_table: compound_elimination_method set to node_specific, but no STP columns for use_STP_elimination_rate defined. Please revise.")
	if(!is.numeric(as.numeric(STP_scenario_year))) stop("Problem in wrap_table: STP_scenario_year not set correctly, please revise.")
	if(!is.null(input_table) & !is.data.frame(input_table)) stop("Problem in wrap_table: input_table must be either NULL or a dataframe")
	if(with_lake_elimination){
		if(!("lake_elimination_min" %in% names(input_table))) stop("Problem in wrap_table: column lake_elimination_min missing in input_table")
		if(!("lake_elimination_max" %in% names(input_table))) stop("Problem in wrap_table: column lake_elimination_max missing in input_table")			
	}
	if(add_absolute_load) if(!("additional_absolut_load" %in% names(input_table))) stop("Problem in wrap_table: column additional_absolut_load missing in input_table")
	if(!("ID" %in% colnames(input_table))) stop("Problem in wrap_table: column ID missing in input_table")
	ID <- as.character(input_table$ID)
	if(!("ID_next" %in% colnames(input_table))) stop("Problem in wrap_table: column ID_next missing in input_table")
	ID_next <- as.character(input_table$ID_next)
	if(!("inhabitants" %in% colnames(input_table))) stop("Problem in wrap_table: column inhabitants missing in input_table")
	inhabitants <- as.numeric(gsub(".", "", as.character(input_table$inhabitants), fixed = TRUE))
	STP_amount_people_local <- input_table$inhabitants
	if(!is.numeric(inhabitants)) stop("Problem in wrap_table: inhabitants must be numeric.")
	inhabitants[is.na(inhabitants)] <- 0	# e.g. for lakes
	if(!identical(length(ID), length(ID_next), length(inhabitants))) stop("Problem in wrap_table: ID, ID_next and inhabitants must be of equal length.")
	if(!is.null(path_out)){
		if(!overwrite) if(file.exists(path_out)) stop("Problem in wrap_table: file at path_out already exists; remove it or use overwrite = TRUE.")
		if(!file.exists(path_out)) dir.create(path = path_out)		
	}
	###############################################			
	
	###############################################
	# reroute STPs depending on STP_scenario_year (re-assing inhabitants, adapt ID_next)
	if(STP_reroute){
		those <- which(
			(input_table[, "type_advanced_treatment"] == "redirection") & 
			(as.numeric(input_table[, "starting_year_advanced_treatment"]) <= STP_scenario_year)
		)
		if(length(those)){
			for(i in those){	
				to_STP <- input_table[i, "redirecting_STP_target_STP_ID"] 		# per ID
				if(!(to_STP %in% input_table$ID)) stop(paste0("Problem in wrap_table: invalid redirecting_STP_target_STP_ID for STP ", input_table[i, "ID"]))
				to_STP <- which(input_table[, "ID"] == to_STP) 					# per table position
				if(!is.na(input_table[to_STP, "redirecting_STP_target_STP_ID"])) stop(paste0("Problem in wrap_table: invalid redirecting_STP_target_STP_ID for STP ", input_table[i, "ID"], ": rerouted STP is rerouted itself."))
				has_STP_amount_people_local <- input_table[i, "inhabitants"]
				input_table[to_STP, "inhabitants"] <- input_table[to_STP, "inhabitants"] + has_STP_amount_people_local
				# if rerouted STP is an ID_next to another STP -> adapt these to its ID_next, if necessary looped in case the latter is rerouted as well
				if(input_table$ID[i] %in% input_table$ID_next){
					for_this_STP <- which(input_table$ID_next == input_table$ID[i])
					to_STP_next <- input_table$ID_next[i]
					at_STP_next <- which(input_table$ID == to_STP_next)
					repeat( 	# if necessary looped in case the latter is rerouted as well
						if(at_STP_next %in% those){
							to_STP_next <- input_table$ID_next[at_STP_next]
							if(is.na(to_STP_next)) break
							at_STP_next <- which(input_table$ID == to_STP_next)							
						}else break
					)
					input_table$ID_next[for_this_STP] <- to_STP_next
				}
			}
			input_table <- input_table[-those,, drop = FALSE]
			ID <- as.character(input_table$ID)
			ID_next <- as.character(input_table$ID_next)
			inhabitants <- as.numeric(gsub(".", "", as.character(input_table$inhabitants), fixed = TRUE))
			inhabitants[is.na(inhabitants)] <- 0	# e.g. for lakes
			STP_amount_people_local <- input_table$inhabitants
		}
	}
	###############################################	
	
	###############################################	
	# get & clean STP treatment steps
	miss_col <- which(!(c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment") %in% names(input_table)))
	if(length(miss_col)){
		stop(
			paste0("Problem in wrap_table: column(s) with name ", 
				paste(c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment"))[miss_col], 
				" missing in input_table")
		)
	}
	STP_treatment_steps <- input_table[, c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment"), drop = FALSE]
	if(!all(STP_treatment_steps[, "nitrification"] %in% c("no", "No", "nein", "Nein", "FALSE", "yes", "Yes", "ja", "Ja", "TRUE", "none"))) stop("Problem in wrap_table: STP_treatment_steps on nitrification are not set correctly. These must be any of no, No, FALSE, or TRUE. Please revise the input table.")
	if(!all(STP_treatment_steps[, "denitrification"] %in% c("no", "No", "nein", "Nein", "FALSE", "yes", "Yes", "ja", "Ja", "TRUE", "none"))) stop("Problem in wrap_table: TP_treatment_steps on denitrification are not set correctly. These must be any of no, No, FALSE, or TRUE. Please revise the input table.")
	if(!all(STP_treatment_steps[, "P_elimination"] %in% c("no", "No", "nein", "Nein", "FALSE", "yes", "Yes", "ja", "Ja", "TRUE", "none"))) stop("Problem in wrap_table: STP_treatment_steps on P_elimination are not set correctly. These must be any of no, No, FALSE, or TRUE. Please revise the input table.")
	STP_treatment_steps[is.na(STP_treatment_steps[, "nitrification"]), "nitrification"] <- "No"	
	STP_treatment_steps[is.na(STP_treatment_steps[, "denitrification"]), "denitrification"] <- "No"	
	STP_treatment_steps[is.na(STP_treatment_steps[, "P_elimination"]), "P_elimination"] <- "No"
	STP_treatment_steps[STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "undefined"), "type_advanced_treatment"] <- NA
	if(STP_filter_steps) STP_treatment_steps[which(as.numeric(STP_treatment_steps[, "starting_year_advanced_treatment"]) > as.numeric(STP_scenario_year)), "type_advanced_treatment"] <- NA		
	###############################################	

	###############################################	
	# run on min/max inputs for nrow(compound_elimination_STP) = 2	
	store_results <- vector("list", 2)
	for(n in seq(2)){
			
		###########################################
		use_columns_local_river_discharge_loop <- use_columns_local_river_discharge[n]
		cols_required <- c(		# all required columns available?
			"ID", "ID_next", "inhabitants", 
			"nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment",
			"redirecting_STP_target_STP_ID", use_columns_local_river_discharge_loop, "lake_elimination_min", "lake_elimination_max"
		)
		if(compound_elimination_method == "node_specific"){
			cols_required <- c(cols_required, use_STP_elimination_rate[n])
			use_columns_STP_elimination_rate_loop <- use_STP_elimination_rate[n]	
		}
		if(any(is.na(match(cols_required, names(input_table))))){
			these_missing <- paste(cols_required[is.na(match(cols_required, names(input_table)))], collapse = ", ")
			stop(paste0("Problem in wrap_table: input_table is missing these columns: ", these_missing))
		}
		STP_local_discharge_river_loop <- as.numeric(input_table[, use_columns_local_river_discharge_loop])
		if(compound_elimination_method == "node_specific"){
			use_STP_elimination_loop <- as.numeric(input_table[, use_columns_STP_elimination_rate_loop])
		}else use_STP_elimination_loop <- FALSE
		compound_load_gramm_per_capita_and_day_loop <- compound_load_gramm_per_capita_and_day[n]
		if(with_lake_elimination){		
			if(n == 1) lake_eliminination_loop <- as.numeric(input_table$lake_elimination_min)
			if(n == 2) lake_eliminination_loop <- as.numeric(input_table$lake_elimination_max)
		}else lake_eliminination_loop <- rep(0, nrow(input_table))
		compound_elimination_STP_loop <- compound_elimination_STP[n,, drop = FALSE]
		if(add_absolute_load) absolute_loads_loop <- input_table$additional_absolut_load else absolute_loads_loop <- rep(0, nrow(input_table))	
		###########################################
		result_table <- SSM:::calc_load(	# calculate local and cumulative loads [g / d]
			ID = ID,
			STP_treatment_steps = STP_treatment_steps,
			ID_next = input_table$ID_next,
			inhabitants = inhabitants,
			STP_elimination_rates = use_STP_elimination_loop,
			compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop,
			compound_elimination_STP = compound_elimination_STP_loop,
			compound_elimination_method = compound_elimination_method,
			with_lake_elimination = with_lake_elimination,
			lake_eliminination_rates = lake_eliminination_loop,
			add_absolute_load = add_absolute_load,
			absolute_loads = absolute_loads_loop
		)
		###########################################
		result_table <- cbind(result_table, 	# append concentrations values
			"conc_local_ug_L" =  (result_table$load_local / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop, 			# ng / L  , load: g/d  discharge: Q347_L_s_kleinster
			"conc_cumulated_ug_L" = (result_table$load_cumulated / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop
		)
		store_results[[n]] <- result_table
		###########################################
		
	}
	names(store_results[[1]]) <- c("ID", "input_load_local_g_d_max", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "node_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")
	names(store_results[[2]]) <- c("ID", "input_load_local_g_d_min", "load_local_g_d_min", "load_cumulated_g_d_min", "inhabitants_cumulated", "node_count_cumulated", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")
	result_table <- cbind(
		store_results[[1]][, c("ID", "input_load_local_g_d_max", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "node_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")],
		store_results[[2]][, c("input_load_local_g_d_min", "load_local_g_d_min", "load_cumulated_g_d_min", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")]	
	)
	###############################################
	
	###############################################
	# calculate topology matrix
	topo_matrix <- SSM:::calc_load(
		ID = ID,
		STP_treatment_steps = STP_treatment_steps,
		ID_next = input_table$ID_next,
		inhabitants = inhabitants,	
		STP_elimination = use_STP_elimination_loop,
		compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop,		# [g / d], set to FALSE to ignore
		compound_elimination_STP = compound_elimination_STP_loop,	# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
		compound_elimination_method = compound_elimination_method,
		with_lake_elimination = with_lake_elimination,
		lake_eliminination = lake_eliminination_loop,	
		add_absolute_load = add_absolute_load,
		absolute_loads = absolute_loads_loop,
		return_data = "matrix"	
	)
	if(!is.null(path_out)){
		done_write <- try({
		
			topo_matrix_export <- topo_matrix
			topo_matrix_export <- rbind(rep("", ncol(topo_matrix_export)), topo_matrix_export)	# shifts the colnames one cell to the left otherwise		
			topo_matrix_export[1, ] <- colnames(topo_matrix)
			topo_matrix_export <- cbind(rep("", nrow(topo_matrix_export)), topo_matrix_export)			
			topo_matrix_export[, 1] <- c("", rownames(topo_matrix))
			write.table(topo_matrix_export, file = file.path(path_out, paste0(scenario_name, "_topo_matrix", ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, 
				row.names = FALSE, 
				col.names = FALSE
			)
			
		})
		if(class(done_write) == "try-error") stop("Problem in wrap_table: qxport of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
	}
	###############################################	
	
	###############################################
	# calculate fraction STP discharge
	STP_local_discharge_L_s  <- STP_amount_people_local * STP_discharge_per_capita / (24 * 60 * 60) 				# convert to [l/s]
	STP_amount_people_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local)
	STP_cumulated_discharge_L_s <- STP_amount_people_cumulated * STP_discharge_per_capita / (24 * 60 * 60) 		# convert to [l/s]
	STP_local_discharge_river <- as.numeric(input_table[, use_columns_local_river_discharge_for_fractions])
	Fraction_STP_discharge_of_river_local <- STP_local_discharge_L_s  / STP_local_discharge_river
	Fraction_STP_discharge_of_river_cumulated <- STP_cumulated_discharge_L_s / STP_local_discharge_river
	Fraction_STP_discharge_of_river_local_includingSTPdischarge <- STP_local_discharge_L_s  / (STP_local_discharge_river + STP_local_discharge_L_s)
	Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge <- STP_cumulated_discharge_L_s / (STP_local_discharge_river + STP_local_discharge_L_s )	
	###############################################
	
	###############################################
	# calculate fraction sewage per upstream treatment step
	classed <- rep(NA, nrow(STP_treatment_steps))
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & 
		(STP_treatment_steps[, "denitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & 
		(STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "undefined"))	
	] <- "only_C_degradation"
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & 
		(STP_treatment_steps[, "denitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & 
		(STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "undefined"))
	] <- "nitrification"	
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & 
		(STP_treatment_steps[, "denitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & 
		(STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "undefined"))
	] <- "denitrification"		
	classed[
		!(STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "undefined"))
	] <- "has_treatment"			
		
	# only_C_degradation
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "only_C_degradation"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / (24 * 60 * 60) 	# convert to [l/s]	
	Fraction_of_wastewater_only_C_removal <- sewage_discharge_cumulated_classed / STP_cumulated_discharge_L_s
	
	# nitrification
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "nitrification"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / (24 * 60 * 60) 	# convert to [l/s]	
	Fraction_of_wastewater_nitrification <- sewage_discharge_cumulated_classed / STP_cumulated_discharge_L_s
	
	# denitrification
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "denitrification"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / (24 * 60 * 60) 	# convert to [l/s]	
	Fraction_of_wastewater_denitrification <- sewage_discharge_cumulated_classed / STP_cumulated_discharge_L_s

	# has_treatment
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "has_treatment"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / (24 * 60 * 60) 	# convert to [l/s]	
	Fraction_of_wastewater_advanced_treatment <- sewage_discharge_cumulated_classed / STP_cumulated_discharge_L_s
	
	# !has_treatment	
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed == "has_treatment"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / (24 * 60 * 60) 	# convert to [l/s]	
	Fraction_of_wastewater_no_advanced_treatment <- sewage_discharge_cumulated_classed / STP_cumulated_discharge_L_s
	
	Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated <- round(STP_local_discharge_river / sewage_discharge_cumulated_classed, digits = 3)		
	Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated_includingSTPdischarge <- Fraction_of_wastewater_no_advanced_treatment * Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge
	# i.e., = sewage_discharge_cumulated_classed / (STP_local_discharge_river + STP_local_discharge_L_s)

	# rowsums for fractions must add up to 1
	has_row_sums <- rowSums(cbind(
			"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
			"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
			"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
			"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment
		))	
	has_row_sums <- round(has_row_sums, digits = 5) # avoid rounding inaccuracies
	if(any(has_row_sums != 1)) stop("Problem in wrap_table: wrong treatment fractions in wrap_table - revise")
	
	Fraction_of_wastewater_only_C_removal <- round(Fraction_of_wastewater_only_C_removal, digits = 3)
	Fraction_of_wastewater_nitrification <- round(Fraction_of_wastewater_nitrification, digits = 3)
	Fraction_of_wastewater_denitrification <- round(Fraction_of_wastewater_denitrification, digits = 3)
	Fraction_of_wastewater_advanced_treatment <- round(Fraction_of_wastewater_advanced_treatment, digits = 3)
	Fraction_of_wastewater_no_advanced_treatment <- round(Fraction_of_wastewater_no_advanced_treatment, digits = 3)
		
	result_table <- cbind(result_table, 
		"STP_local_discharge_L_s" = inhabitants * STP_discharge_per_capita / (24 * 60 * 60),
		"STP_cumulated_discharge_L_s" = STP_cumulated_discharge_L_s,
		"Fraction_STP_discharge_of_river_local" = Fraction_STP_discharge_of_river_local,
		"Fraction_STP_discharge_of_river_cumulated" = Fraction_STP_discharge_of_river_cumulated,
		"Fraction_STP_discharge_of_river_local_includingSTPdischarge" = Fraction_STP_discharge_of_river_local_includingSTPdischarge,
		"Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge" = Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge,
		"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
		"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
		"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
		"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment,
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated_includingSTPdischarge" = Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated_includingSTPdischarge,
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated" = Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated
	)
	###############################################	
	
	###############################################
	# format, export & return
	result_table <- result_table[, c( 	# -> reorder table
		"ID",
		"inhabitants_cumulated",
		"STP_local_discharge_L_s",
		"STP_cumulated_discharge_L_s",
		"node_count_cumulated",
		"Fraction_STP_discharge_of_river_local",
		"Fraction_STP_discharge_of_river_cumulated",
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated",
		"Fraction_STP_discharge_of_river_local_includingSTPdischarge",
		"Fraction_STP_discharge_of_river_cumulated_includingSTPdischarge",
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated_includingSTPdischarge",
		"Fraction_of_wastewater_only_C_removal",
		"Fraction_of_wastewater_nitrification",
		"Fraction_of_wastewater_denitrification",
		"Fraction_of_wastewater_advanced_treatment",
		"input_load_local_g_d_max", 
		"input_load_local_g_d_min",
		"load_local_g_d_max",
		"load_local_g_d_min",
		"load_cumulated_g_d_max",
		"load_cumulated_g_d_min",
		"conc_local_ug_L_max",
		"conc_local_ug_L_min",			
		"conc_cumulated_ug_L_max",
		"conc_cumulated_ug_L_min"			
	), drop = FALSE]	
	
	if(is.null(path_out)) return(result_table) else{
		if(file.exists(file.path(path_out, paste0(scenario_name, ".csv"))) & !overwrite) stop("Problem in wrap_table: file at path_out already exists, and overwrite is set to FALSE")
		
		# append infos to result_table
		if(length(add_columns_from_input_table)){
		
			use_cols <- match(add_columns_from_input_table, names(input_table))
			if(any(is.na(use_cols))) stop(paste0("Problem in wrap_table: invalid add_columns_from_input_table on ", add_columns_from_input_table[is.na(use_cols)]))

			use_rows <- match(input_table[, "ID"], result_table[, "ID"])
			result_table <- cbind(
				"ID" = result_table[, "ID"], 
				input_table[use_rows, use_cols], 
				result_table[, names(result_table) != "ID"]
			)
		}
		
		result_table <- rbind(
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			names(result_table),
			result_table
		)
		
		names(result_table) <- NULL
		result_table[2, 2] <- "compound name:"
		result_table[3, 2] <- compound_name	
		result_table[2, 3] <- "compound load [g/In d]):"
		result_table[3, 3] <- paste(compound_load_gramm_per_capita_and_day, collapse = ", ")
		result_table[2, 5] <- "scenario year:"
		result_table[3, 5] <- STP_scenario_year
		result_table[1, 7] <- "elimitation rates"
		result_table[2, 7] <- "nitrification:"
		result_table[3, 7] <- paste(compound_elimination_STP$nitrification, collapse = ", ")
		result_table[2, 8] <- "denitrification:"
		result_table[3, 8] <- paste(compound_elimination_STP$denitrification, collapse = ", ")
		result_table[2, 9] <- "P_elimination:"
		result_table[3, 9] <- paste(compound_elimination_STP$P_elimination, collapse = ", ")
		result_table[2, 10] <- "GAC:"
		result_table[3, 10] <- paste(compound_elimination_STP$GAC, collapse = ", ")
		result_table[2, 11] <- "combi:"
		result_table[3, 11] <- paste(compound_elimination_STP$combi, collapse = ", ")
		result_table[2, 12] <- "ozonation:"
		result_table[3, 12] <- paste(compound_elimination_STP$ozonation, collapse = ", ")
		result_table[2, 13] <- "PAC:"
		result_table[3, 13] <- paste(compound_elimination_STP$PAC, collapse = ", ")
		result_table[2, 14] <- "undefined:"
		result_table[3, 14] <- paste(compound_elimination_STP$Ausbau, collapse = ", ")
		result_table[2, 15] <- "COD_treatment"
		result_table[3, 15] <- paste(compound_elimination_STP$COD_treatment, collapse = ", ")		
		result_table[1, 17] <- "parameters" 		
		result_table[2, 17] <- "compound elimination method"		
		result_table[3, 17] <- as.character(compound_elimination_method)
		result_table[2, 18] <- "redirection activ?"
		result_table[3, 18] <- as.character(STP_reroute)
		result_table[2, 19] <- "filtering treatment steps?"
		result_table[3, 19] <- as.character(STP_filter_steps)
		result_table[2, 20] <- "lake elimination enabled?"
		result_table[3, 20] <- paste(with_lake_elimination, collapse = ", ")	
		result_table[2, 21] <- "use_columns_local_river_discharge"		
		result_table[3, 21] <- paste(use_columns_local_river_discharge, collapse = ", ")	
		result_table[2, 22] <- "use_columns_local_river_discharge_for_fractions"		
		result_table[3, 22] <- paste(use_columns_local_river_discharge_for_fractions, collapse = ", ")	
		result_table[2, 23] <- "STP_discharge_per_capita"		
		result_table[3, 23] <- as.character(STP_discharge_per_capita)

		if(write_csv){
			done_write <- try({
				write.table(result_table, file = file.path(path_out, paste0(scenario_name, ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, row.names = FALSE)
			})
			if(class(done_write) == "try-error") stop("Problem in wrap_table: export of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
		}else{
			done_write <- try({
				wb <- openxlsx:::createWorkbook()	
				openxlsx:::addWorksheet(wb, scenario_name)
				openxlsx:::writeData(wb, scenario_name, result_table, startCol = 2, startRow = 3, rowNames = FALSE)
				openxlsx:::saveWorkbook(wb, file = file.path(path_out, paste0(scenario_name, ".xlsx")), overwrite = TRUE)
			})
			if(class(done_write) == "try-error") stop("Problem in wrap_table: export of results to path_out.xlsx failed. Is this path valid? Is the file open in another software?")
		}
	
	}
	###############################################	
	
}



