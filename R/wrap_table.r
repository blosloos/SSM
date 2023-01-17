


wrap_table <- function(

	STP_table = NULL,									# Must be a data.frame if provided, overwrites all of the below STP_ arguments
	STP_scenario_year = 2021,
	STP_reroute = TRUE,									# Reroute STPs until a given STP_scenario_year
	STP_filter_steps = TRUE,							# Filter STP treatment steps until a given STP_scenario_year
	STP_discharge_per_capita = 400,						# [l / E d]
	compound_name = "not_specified",
	scenario_name = compound_name,
	compound_load_total = FALSE, 						# [kg / a]
	compound_load_gramm_per_capita_and_day,				# [g / E d], set to FALSE to ignore
	compound_elimination_STP = NULL,					# named dataframe or vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_elimination_method = "micropollutants",	# "micropollutants" or "STP individual"
	with_lake_elimination = FALSE,
	add_absolute_load = FALSE,
	use_columns_local_discharge = c("Q347_L_s_min", "Q347_L_s_max"),
	use_columns_local_discharge_for_fractions = "Q347_L_s_min", 
	use_STP_elimination_rate = FALSE,
	add_columns_from_STP_table = c("STP_next", "X_position", "Y_position"),
	path_out = FALSE,									# if FALSE, return data.frame
	overwrite = TRUE,
	write_csv = TRUE,									# else, exports an excel file
	use_sep_csv = " "
	
){

	###############################################
	# checks / extract STP_table columns
	if(!identical(
		length(compound_load_gramm_per_capita_and_day),
		length(use_columns_local_discharge),
		nrow(compound_elimination_STP)
	)) stop("compound_load_gramm_per_capita_and_day, use_columns_local_discharge and the number of rows in compound_elimination_STP must be of equal length")	
	
	if(length(compound_load_gramm_per_capita_and_day) != 2) stop("compound_load_gramm_per_capita_and_day, use_columns_local_discharge and the number of rows in compound_elimination_STP must have two entries")
	
	if(length(compound_load_gramm_per_capita_and_day) == 2){
		compound_load_gramm_per_capita_and_day <- sort(compound_load_gramm_per_capita_and_day,  decreasing = TRUE)
		if(any(compound_elimination_STP[1, ] > compound_elimination_STP[2, ])) stop("compound_elimination_STP set incorrectly for range calculation")
		if(any(STP_table[, use_columns_local_discharge[1]] > STP_table[, use_columns_local_discharge[2]], na.rm = TRUE)) stop("use_columns_local_discharge set incorrectly for range calculation")	
	}
	
	if((compound_elimination_method == "STP individual") & (use_STP_elimination_rate[1] == FALSE)) stop("compound_elimination_method set to STP individual, but no STP columns for use_STP_elimination_rate defined. Please revise.")
	
	if(!is.numeric(STP_scenario_year)) stop("STP_scenario_year must be numeric")
	
	if(!is.null(STP_table) & !is.data.frame(STP_table)) stop("STP_table must be either NULL or a dataframe")
	
	if(with_lake_elimination){
		if(!("lake_elimination_min" %in% names(STP_table))) stop("Column lake_elimination_min missing in STP_table")
		if(!("lake_elimination_max" %in% names(STP_table))) stop("Column lake_elimination_max missing in STP_table")			
	}
	
	if(add_absolute_load) if(!("additional_absolut_load" %in% names(STP_table))) stop("Column additional_absolut_load missing in STP_table")
	
	if(!("ID" %in% colnames(STP_table))) stop("Column ID missing in STP_table")
	STP_id <- as.character(STP_table$ID)
	
	if(!("STP_next" %in% colnames(STP_table))) stop("Column STP_next missing in STP_table")
	STP_id_next <- as.character(STP_table$STP_next)
	
	if(!("inhabitants" %in% colnames(STP_table))) stop("Column inhabitants missing in STP_table")
	STP_amount_inhabitants <- as.numeric(gsub(".", "", as.character(STP_table$inhabitants), fixed = TRUE))
	STP_amount_people_local <- STP_table$inhabitants
	if(!is.numeric(STP_amount_inhabitants)) stop("Problem in wrap_table: STP_amount_inhabitants must be numeric.")
	STP_amount_inhabitants[is.na(STP_amount_inhabitants)] <- 0	# e.g. for lakes
	
	if(!identical(length(STP_id), length(STP_id_next), length(STP_amount_inhabitants))) stop("Problem in wrap_table: STP_id, STP_id_next and STP_amount_inhabitants must be of equal length.")
	if(!overwrite & !is.logical(path_out)) if(file.exists(path_out)) stop("Problem in wrap_table: file at path_out already exists; remove it or use overwrite = TRUE.")
	if(!file.exists(path_out)) dir.create(path = path_out)		
	###############################################			
	
	###############################################
	# reroute STPs depending on STP_scenario_year
	# -> re-assing inhabitants, adapt STP_next
	if(STP_reroute){
	
		those <- which(
			(STP_table[, "type_advanced_treatment"] == "redirection") & 
			(as.numeric(STP_table[, "starting_year_advanced_treatment"]) <= STP_scenario_year)
		)
		if(length(those)){
			for(i in those){	
			
				to_STP <- STP_table[i, "redirecting_STP_target_STP_ID"] 		# per ID
				if(!(to_STP %in% STP_table$ID)) stop(paste0("Invalid redirecting_STP_target_STP_ID for STP ", STP_table[i, "ID"]))
				to_STP <- which(STP_table[, "ID"] == to_STP) 					# per table position
				if(!is.na(STP_table[to_STP, "redirecting_STP_target_STP_ID"])) stop(paste0("Invalid redirecting_STP_target_STP_ID for STP ", STP_table[i, "ID"], ": rerouted STP is rerouted itself."))
				has_STP_amount_people_local <- STP_table[i, "inhabitants"]
				STP_table[to_STP, "inhabitants"] <- STP_table[to_STP, "inhabitants"] + has_STP_amount_people_local
				# if rerouted STP is an STP_next to another STP -> adapt these to its STP_next, if necessary looped in case the latter is rerouted as well
				if(STP_table$ID[i] %in% STP_table$STP_next){
				
					for_this_STP <- which(STP_table$STP_next == STP_table$ID[i])
					to_STP_next <- STP_table$STP_next[i]
					at_STP_next <- which(STP_table$ID == to_STP_next)
					repeat( 	# if necessary looped in case the latter is rerouted as well
						if(at_STP_next %in% those){
							to_STP_next <- STP_table$STP_next[at_STP_next]
							if(is.na(to_STP_next)) break
							at_STP_next <- which(STP_table$ID == to_STP_next)							
						}else break
					)
					STP_table$STP_next[for_this_STP] <- to_STP_next
				
				}
			}
			STP_table_rerouted <- STP_table[those,, drop = FALSE]
			STP_table <- STP_table[-those,, drop = FALSE]
			
		}else STP_table_rerouted <- NULL
		
	}
	###############################################	
	
	###############################################	
	# get & clean STP treatment steps
	miss_col <- which(!(c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment") %in% names(STP_table)))
	if(length(miss_col)){
		stop(
			paste0("Column(s) withe name ", 
				paste(c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment"))[miss_col], 
				" missing in STP_table")
		)
	}
	STP_treatment_steps <- STP_table[, c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment"), drop = FALSE]
	
	
# ->WORK HERE:	
	STP_treatment_steps %in% c("no", "No", "nein", "Nein", "FALSE", "yes", "Yes", "ja", "Ja", "TRUE")
	
	
	
	STP_treatment_steps[is.na(STP_treatment_steps[, "nitrification"]), "nitrification"] <- "No"	
	STP_treatment_steps[is.na(STP_treatment_steps[, "denitrification"]), "denitrification"] <- "No"	
	STP_treatment_steps[is.na(STP_treatment_steps[, "P_elimination"]), "P_elimination"] <- "No"
	STP_treatment_steps[STP_treatment_steps[, "type_advanced_treatment"] %in% c("redirection", "Umleitung wahrscheinlich"), "type_advanced_treatment"] <- NA
	if(STP_filter_steps) STP_treatment_steps[which(STP_treatment_steps[, "starting_year_advanced_treatment"] > STP_scenario_year), "type_advanced_treatment"] <- NA		
	###############################################	

	###############################################	
	# run on min/max inputs for nrow(compound_elimination_STP) = 2	
	store_results <- vector("list", nrow(compound_elimination_STP))
	for(n in 1:nrow(compound_elimination_STP)){
			
		###########################################
		use_columns_local_discharge_loop <- use_columns_local_discharge[n]
		cols_required <- c(		# all required columns available?
			"ID", "STP_next", "inhabitants", 
			"nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment",
			"redirecting_STP_target_STP_ID", use_columns_local_discharge_loop, "lake_elimination_min", "lake_elimination_max"
		)
		
		if(compound_elimination_method == "STP individual"){
			cols_required <- c(cols_required, use_STP_elimination_rate[n])
			use_columns_STP_elimination_rate_loop <- use_STP_elimination_rate[n]	
		}
		
		if(any(is.na(match(cols_required, names(STP_table))))){
			these_missing <- paste(cols_required[is.na(match(cols_required, names(STP_table)))], collapse = ", ")
			stop(paste0("STP_table is missing these columns: ", these_missing))
		}
		
		STP_local_discharge_river_loop <- as.numeric(STP_table[, use_columns_local_discharge_loop])
		
		if(compound_elimination_method == "STP individual"){
			use_STP_elimination_rate_loop <- as.numeric(STP_table[, use_columns_STP_elimination_rate_loop])
		}else use_STP_elimination_rate_loop <- FALSE
		
		compound_load_gramm_per_capita_and_day_loop <- compound_load_gramm_per_capita_and_day[n]
		
		if(with_lake_elimination){		
			if(n == 1) lake_eliminination_rates_loop <- as.numeric(STP_table$lake_elimination_min)
			if(n == 2) lake_eliminination_rates_loop <- as.numeric(STP_table$lake_elimination_max)
		}else lake_eliminination_rates_loop <- rep(0, nrow(STP_table))
		
		compound_elimination_STP_loop <- compound_elimination_STP[n,, drop = FALSE]
	
		if(add_absolute_load) absolute_loads_loop <- STP_table$additional_absolut_load else absolute_loads_loop <- rep(0, nrow(STP_table))	
		
		result_table <- SSM:::calc_load(	# calculate local and cumulative loads [g / d]
			inhabitants_total = sum(STP_amount_inhabitants),
			STP_id = STP_id,
			STP_treatment_steps = STP_treatment_steps,
			STP_amount_inhabitants = STP_amount_inhabitants,
			STP_elimination_rates = use_STP_elimination_rate_loop,
			compound_load_total = FALSE,
			compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop,
			compound_elimination_STP = compound_elimination_STP_loop,
			compound_elimination_method = compound_elimination_method,
			with_lake_elimination = with_lake_elimination,
			lake_eliminination_rates = lake_eliminination_rates_loop,
			add_absolute_load = add_absolute_load,
			absolute_loads = absolute_loads_loop,
			STP_next = STP_table$STP_next
		)
		
if(FALSE){		

			inhabitants_total = sum(STP_amount_inhabitants)
			STP_id = STP_id
			STP_treatment_steps = STP_treatment_steps
			STP_amount_inhabitants = STP_amount_inhabitants	
			STP_elimination_rates = use_STP_elimination_rate_loop
			compound_load_total = FALSE
			compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop
			compound_elimination_STP = compound_elimination_STP_loop
			compound_elimination_method = compound_elimination_method
			with_lake_elimination = with_lake_elimination
			lake_eliminination_rates = lake_eliminination_rates_loop
			add_absolute_load = add_absolute_load
			absolute_loads = absolute_loads_loop
			STP_next = STP_table$STP_next		
}		

		result_table <- cbind(result_table, 	# append concentrations values
			"conc_local_ug_L" =  (result_table$load_local / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop, 			# ng / L  , load: g/d  discharge: Q347_L_s_kleinster
			"conc_cumulated_ug_L" = (result_table$load_cumulated / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop
		)
		###########################################		
		store_results[[n]] <- result_table
		
	}
	if(nrow(compound_elimination_STP) == 2){
	
		names(store_results[[1]]) <- c("STP_ID", "input_load_local_g_d_max", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")
		names(store_results[[2]]) <- c("STP_ID", "input_load_local_g_d_min", "load_local_g_d_min", "load_cumulated_g_d_min", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")
		result_table <- cbind(
			store_results[[1]][, c("STP_ID", "input_load_local_g_d_max", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")],
			store_results[[2]][, c("input_load_local_g_d_min", "load_local_g_d_min", "load_cumulated_g_d_min", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")]	
		)
	
	}else result_table <- store_results[[1]]
	###############################################
	
	###############################################
	# calculate topology matrix ###################
	topo_matrix <- SSM:::calc_load(
		inhabitants_total = sum(STP_amount_inhabitants),								# Set to FALSE to ignore
		STP_id = STP_id,
		STP_treatment_steps = STP_treatment_steps,
		STP_amount_inhabitants = STP_amount_inhabitants,	
		STP_elimination_rates = use_STP_elimination_rate_loop,
		compound_load_total = FALSE, 								# [kg / a]
		compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop,		# [g / E d], set to FALSE to ignore
		compound_elimination_STP = compound_elimination_STP_loop,	# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
		compound_elimination_method = compound_elimination_method,
		with_lake_elimination = with_lake_elimination,
		lake_eliminination_rates = lake_eliminination_rates_loop,	
		add_absolute_load = add_absolute_load,
		absolute_loads = absolute_loads_loop,
		STP_next = STP_table$STP_next,
		return_data = "matrix"	
	)
	if(!is.logical(path_out)){
		done_write <- try({
		
			topo_matrix_export <- topo_matrix
			topo_matrix_export <- rbind(rep("", ncol(topo_matrix_export)), topo_matrix_export)	# shifts the colnames one cell to the left otherwise		
			topo_matrix_export[1, ] <- colnames(topo_matrix)
			topo_matrix_export <- cbind(rep("", nrow(topo_matrix_export)), topo_matrix_export)			
			topo_matrix_export[, 1] <- c("", rownames(topo_matrix))
			write.table(topo_matrix_export, file = file.path(path_out, paste0("topo_matrix_", scenario_name, ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, 
				row.names = FALSE, 
				col.names = FALSE
			)
			
		})
		if(class(done_write) == "try-error") stop("Export of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
	}
	###############################################	
	
	###############################################
	# STP_discharge_L_s	
	result_table <- cbind(result_table, 
		"STP_discharge_L_s" = (STP_amount_inhabitants * STP_discharge_per_capita / (24 * 60 * 60))
	)
	###############################################
	
	###############################################
	# fraction STP discharge ######################
	sewage_discharge_local <- STP_amount_people_local * STP_discharge_per_capita / 24 / 60 / 60 				# convert to [l/s]
	STP_amount_people_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local)
	sewage_discharge_cumulated <- STP_amount_people_cumulated * STP_discharge_per_capita / 24 / 60 / 60 		# convert to [l/s]
	
	STP_local_discharge_river <- as.numeric(STP_table[, use_columns_local_discharge_for_fractions])
	
	Discharge_ratio_river_to_STP_local <- STP_local_discharge_river / sewage_discharge_local
	Discharge_ratio_river_to_STP_cumulated <- STP_local_discharge_river / sewage_discharge_cumulated
	
	Fraction_STP_discharge_of_river_local <- sewage_discharge_local / ( STP_local_discharge_river + sewage_discharge_local )
	Fraction_STP_discharge_of_river_cumulated <- sewage_discharge_cumulated / ( STP_local_discharge_river + sewage_discharge_local )	
	
	result_table <- cbind(result_table, 
		"Discharge_ratio_river_to_STP_local" = Discharge_ratio_river_to_STP_local,
		"Discharge_ratio_river_to_STP_cumulated" = Discharge_ratio_river_to_STP_cumulated,
		"Fraction_STP_discharge_of_river_local" = Fraction_STP_discharge_of_river_local,
		"Fraction_STP_discharge_of_river_cumulated" = Fraction_STP_discharge_of_river_cumulated
	)
	###############################################
	
	###############################################
	# fraction sewage per upstream treatment step
	# -> Test further below: rowsums must be equal to 1
	
	classed <- rep(NA, nrow(STP_treatment_steps))
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & (STP_treatment_steps[, "denitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & is.na(STP_treatment_steps[, "type_advanced_treatment"])
	] <- "nur_C_Abbau"
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & (STP_treatment_steps[, "denitrification"] %in% c("no", "No", "nein", "Nein", "FALSE")) & is.na(STP_treatment_steps[, "type_advanced_treatment"])
	] <- "nitrification"	
	classed[
		(STP_treatment_steps[, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & (STP_treatment_steps[, "denitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) & is.na(STP_treatment_steps[, "type_advanced_treatment"])
	] <- "denitrification"		
	classed[
		!is.na(STP_treatment_steps[, "type_advanced_treatment"])
	] <- "MV_Behandlung"			
	classed[is.na(classed)] <- "Sonstige"
	
	# nur_C_Abbau
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "nur_C_Abbau"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_only_C_removal <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# nitrification
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "nitrification"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_nitrification <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# denitrification
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "denitrification"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_denitrification <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)

	# MV_Behandlung
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "MV_Behandlung"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_advanced_treatment <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# Sonstige
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "Sonstige"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_Sonstige <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# keine MV_Behandlung
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed == "MV_Behandlung"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_no_advanced_treatment <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated <- round(STP_local_discharge_river / sewage_discharge_cumulated_classed, digits = 3)	
		
	# -> Test: rowsum für Anteile muss 1 ergeben
	if(FALSE) if(any(rowSums(cbind(
			"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
			"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
			"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
			"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment
		)) != 1)) stop("Missing treatment fractions in wrap_table - revise")
	
	result_table <- cbind(result_table, 
		"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
		"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
		"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
		"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment
	)
	
	Fraction_of_river_only_C_removal <- Fraction_of_wastewater_only_C_removal * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_nitrification <- Fraction_of_wastewater_nitrification * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_denitrification <-  Fraction_of_wastewater_denitrification * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_advanced_treatment <- Fraction_of_wastewater_advanced_treatment * Fraction_STP_discharge_of_river_cumulated	
	Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated <- Fraction_of_wastewater_no_advanced_treatment * Fraction_STP_discharge_of_river_cumulated	
	
	
	result_table <- cbind(result_table, 
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated" = Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated,
		"Fraction_of_river_only_C_removal" = Fraction_of_river_only_C_removal,
		"Fraction_of_river_nitrification" = Fraction_of_river_nitrification,
		"Fraction_of_river_denitrification" = Fraction_of_river_denitrification,
		"Fraction_of_river_advanced_treatment" = Fraction_of_river_advanced_treatment,
		"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated" = Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated
	)	
	
	###############################################
	# format, export & return #####################	
	
	if(nrow(compound_elimination_STP) == 2){
	
		# -> reorder table
		result_table <- result_table[, c( 
			"STP_ID",
			"inhabitants_cumulated",
			"STP_discharge_L_s",
			"STP_count_cumulated",
			"Discharge_ratio_river_to_STP_local",
			"Discharge_ratio_river_to_STP_cumulated",
			"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated",
			"Fraction_STP_discharge_of_river_local",
			"Fraction_STP_discharge_of_river_cumulated",
			"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated",
			"Fraction_of_river_only_C_removal",
			"Fraction_of_river_nitrification",
			"Fraction_of_river_denitrification",
			"Fraction_of_river_advanced_treatment",
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
	
	}else{
	
		# -> reorder table
		result_table <- result_table[, c( 
			"STP_ID",
			"inhabitants_cumulated",
			"STP_discharge_L_s",
			"STP_count_cumulated",
			"Discharge_ratio_river_to_STP_local",
			"Discharge_ratio_river_to_STP_cumulated",
			"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated",
			"Fraction_STP_discharge_of_river_local",
			"Fraction_STP_discharge_of_river_cumulated",
			"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated",
			"Fraction_of_river_only_C_removal",
			"Fraction_of_river_nitrification",
			"Fraction_of_river_denitrification",
			"Fraction_of_river_advanced_treatment",
			"Fraction_of_wastewater_only_C_removal",
			"Fraction_of_wastewater_nitrification",
			"Fraction_of_wastewater_denitrification",
			"Fraction_of_wastewater_advanced_treatment",
			"input_load_local_g_d",
			"load_local_g_d",
			"load_cumulated_g_d",
			"conc_local_ug_L",
			"conc_cumulated_ug_L"
		), drop = FALSE]
	
	}
		
	if(is.logical(path_out)) return(result_table) else{
		if(file.exists(file.path(path_out, paste0("STP_result_", scenario_name, ".csv"))) & !overwrite) stop("File at path_out already exists, and overwrite is set to FALSE")
	
		# add more STP infos to result_table
		use_cols <- match(add_columns_from_STP_table, names(STP_table))
		use_rows <- match(STP_table[, "ID"], result_table[, "STP_ID"])
		result_table <- cbind(
			"STP_ID" = result_table[, "STP_ID"], 
			STP_table[use_rows, use_cols], 
			result_table[, names(result_table) != "STP_ID"]
		)
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
		result_table[2, 14] <- "Ausbau:"
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
		
		result_table[2, 21] <- "use_columns_local_discharge"		
		result_table[3, 21] <- paste(use_columns_local_discharge, collapse = ", ")	
		
		result_table[2, 22] <- "use_columns_local_discharge_for_fractions"		
		result_table[3, 22] <- paste(use_columns_local_discharge_for_fractions, collapse = ", ")	

		result_table[2, 23] <- "STP_discharge_per_capita"		
		result_table[3, 23] <- as.character(STP_discharge_per_capita)

		if(write_csv){
		
			done_write <- try({
			
				write.table(result_table, file = file.path(path_out, paste0("STP_result_", scenario_name, ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, row.names = FALSE)
			
			})
			if(class(done_write) == "try-error") stop("Export of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
		
		}else{
		
			done_write <- try({
			
				wb <- openxlsx:::createWorkbook()	
				openxlsx:::addWorksheet(wb, scenario_name)
				openxlsx:::writeData(wb, scenario_name, result_table, startCol = 2, startRow = 3, rowNames = FALSE)
				openxlsx:::saveWorkbook(wb, file = file.path(path_out, paste0("STP_result_", scenario_name, ".xlsx")), overwrite = TRUE)
			
			})
			if(class(done_write) == "try-error") stop("Export of results to path_out.xlsx failed. Is this path valid? Is the file open in another software?")
	
		}
	
	}
	###############################################	
	
}



