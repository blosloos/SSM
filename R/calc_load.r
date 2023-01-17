






calc_load <- function(

	inhabitants_total = 8417700,
	STP_id,
	STP_treatment_steps,
	STP_next,
	STP_amount_inhabitants,	
	STP_elimination_rates = FALSE,
	compound_load_total = FALSE, 						# [kg / a], set to FALSE to ignore and then use compound_load_gramm_per_capita_and_day
	compound_load_gramm_per_capita_and_day = NULL,		# [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_elimination_STP = NULL,					# named dataframe with elimination fractions over treatment steps (not percentage values); set all to 0 to skip a step 
	compound_elimination_method = "micropollutants",	# "micropollutants" or "STP individual
	with_lake_elimination = FALSE,
	lake_eliminination_rates = NULL,
	add_absolute_load = FALSE,
	absolute_loads = FALSE,
	return_data = "loads"								# "loads" or "matrix"
	
){

	###############################################
	# check inputs & defaults
	
	if(!is.data.frame(compound_elimination_STP)) stop("Problem in wrap_table, argument compound_elimination_STP is not a dataframe.")
	if(any(!sapply(compound_elimination_STP, is.numeric))) stop("Problem in wrap_table, dataframe compound_elimination_STP has non-numeric entries.")
	STP_steps <- c("COD_treatment", "nitrification", "denitrification", "P_elimination", "GAC", "combi", "ozonation", "PAC", "Ausbau")
	not_found <- !(names(compound_elimination_STP) %in% STP_steps)
	if(any(not_found)) stop(paste0("Problem in wrap_table, argument compound_elimination_STP: entry ", paste(names(compound_elimination_STP)[not_found], collapse = ", "), " is missing."))
	if(any((compound_elimination_STP < 0) & (compound_elimination_STP > 1))) stop("Problem in calc_load: compound_elimination_STP not within [0,1]")
	if(!(compound_elimination_method %in% c("micropollutants", "STP individual"))) stop("Problem in calc_load: invalid compound_elimination_method, must be either micropollutants or STP individual.")
	
	if((compound_elimination_method == "STP individual") & (STP_elimination_rates[1] == FALSE)) stop("Problem in calc_load: compound_elimination_method set to STP individual, but no STP_elimination_rates provided. Please revise.")
	

	if(!identical(length(STP_id), length(STP_amount_inhabitants))) stop("Problem in calc_load: STP inputs vary in length_1")
	
	if(nrow(STP_treatment_steps)) != length(STP_id)) stop("Problem in calc_load: number of rows for STP_treatment_steps not equal to length of STP_id") 

	if(length(compound_load_total) > 1) stop("Problem in calc_load: compound_load_total should consist of one value only")
	if(length(compound_load_gramm_per_capita_and_day) > 1) stop("Problem in calc_load: compound_load_gramm_per_capita_and_day should consist of one value only")
	if(!is.numeric(compound_load_total) & !is.numeric(compound_load_gramm_per_capita_and_day)) stop("Problem in calc_load: either compound_load_total or compound_load_gramm_per_capita_and_day must be defined.")

	that_not <- which(!(STP_next[!is.na(STP_next)] %in% STP_id))
	if(length(that_not)) stop(paste0("Invalid STP_next entry detected: ", paste(STP_next[!is.na(STP_next)][that_not], collapse = ", ")))

	if(any(is.na(STP_amount_inhabitants))) stop("Problem in calc_load: STP_amount_inhabitants contains NAs")

	###############################################
	if(!is.numeric(compound_load_gramm_per_capita_and_day)) compound_load_gramm_per_capita_and_day <- compound_load_total * 1000 / inhabitants_total / 365 		# [kg/a] -> [g/d]


	
	compound_elimination_STP_calc <- rep(0, nrow(STP_treatment_steps))
	for(i in 1:nrow(STP_treatment_steps)){

		#######################################
		if(compound_elimination_method == "micropollutants"){
		
			# STP, See <- all STP_treatment_steps set to "no"
			# <- ADD TEST
			
			compound_elimination_STP_calc[i] <- prod(1 - c(				
				
				if(STP_treatment_steps[i, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$nitrification else compound_elimination_STP$COD_treatment,
								
				# denitrification should only be available if there is a prior nitrification, too - not further checked
				if(STP_treatment_steps[i, "denitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$denitrification,
					
				if(STP_treatment_steps[i, "P_elimination"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$P_elimination,		
					
				if(!is.na(STP_treatment_steps[i, "type_advanced_treatment"])){
					compound_elimination_STP[
						names(compound_elimination_STP) == STP_treatment_steps[i, "type_advanced_treatment"]
					][[1]]
				}else 0
					
			))		
			if(is.na(compound_elimination_STP_calc[i])) stop("Problem in calc_load: NA for compound_elimination_STP_calc detected.")
		}
		#######################################
		if(compound_elimination_method == "STP individual"){ 
				
			if(!length(STP_elimination_rates[i])) next
			if(is.na(STP_elimination_rates[i])) next
			if(is.na(as.numeric(STP_elimination_rates[i]))) next
			compound_elimination_STP_calc[i] <- (1 - as.numeric(STP_elimination_rates[i]))
		
		}
		#######################################
		
	}
		

	
	input_load_local_g_d <- STP_amount_inhabitants * compound_load_gramm_per_capita_and_day 
	load_local_g_d <- STP_amount_inhabitants * compound_load_gramm_per_capita_and_day * compound_elimination_STP_calc
	
	load_local_g_d[is.na(load_local_g_d)] <- 0 # for lakes
	
	if(add_absolute_load){
		absolute_loads[is.na(absolute_loads)] <- 0
		load_local_g_d <- load_local_g_d + absolute_loads
	}
	
	if(!with_lake_elimination) lake_eliminination_rates <- rep(0, length(lake_eliminination_rates)) else{
	
		lake_eliminination_rates[is.na(as.numeric(lake_eliminination_rates))] <- 0
		if(any(lake_eliminination_rates < 0) | any(lake_eliminination_rates > 1)) stop("Problem in calc_load: lake_eliminination_rate must be within [0, 1]")
		if(compound_elimination_method == "STP individual") if(any(lake_eliminination_rates != 0 & STP_elimination_rates != 0 )) stop("
			Problem in calc_load: lake_eliminination_rates and STP_elimination_rates must be exclusive: not both can eliminate together.")
	
	}
	
	# init cumulative loads with local loads
	load_cumulated_g_d <- load_local_g_d
	
	not_loop_endless <- 0 # just to be save
		
	STP_next_iter <- STP_next
	do_calc_node <- rep(TRUE, length(STP_id))
	
	topo_matrix <- matrix(nrow = length(STP_id), ncol = length(STP_id), 0)
	colnames(topo_matrix) <- rownames(topo_matrix) <- STP_id
	
	
	while(
		any(do_calc_node) &
		not_loop_endless < 1E5
	){		
		
		for(k in 1:length(STP_id)){ # check through STPs / lakes
	
			# (1) still required to be calculated?
			if(!do_calc_node[k]) next
			
			# (2) still awaiting load input, i.e., still another STP points to STP_id[k]?
			if(STP_id[k] %in% STP_next_iter) next		
	
			# (3) for lakes: reduce load before adding on
			load_cumulated_g_d[k] <- load_cumulated_g_d[k] * (1 - lake_eliminination_rates[k])		

			# (4) adding on load to next STP or lake
			if(!is.na(STP_next_iter[k])){
				load_cumulated_g_d[STP_id == STP_next_iter[k]] <- load_cumulated_g_d[STP_id == STP_next_iter[k]] + load_cumulated_g_d[k]
				topo_matrix[
					rownames(topo_matrix) == STP_id[k],
					colnames(topo_matrix) == STP_next_iter[k]
				] <- 1
				STP_next_iter[k] <- NA	# for point (2)
			}

			# (5) mark that node has been done
			do_calc_node[k] <- FALSE
		
		}
			
		not_loop_endless <- not_loop_endless + 1
			
	}
	
	if(not_loop_endless >= 1E5) stop("Load routing through STP and lake network did not finish within expected number of iterations (check STP_next for circular pointing).") 
	
	###############################################
	len <- length(STP_id)
	topo_matrix <- solve(diag(len) - topo_matrix)
	if(!sum(topo_matrix)) stop("Problem in calc_load: no relations between STPs found. Is STP_next all set to 0?")	
	if(return_data == "matrix")	return(topo_matrix)
	###############################################	
	
	###############################################	
	inhabitants_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = STP_amount_inhabitants)
	STP_count_cumulated <- apply(topo_matrix, MARGIN = 2, function(x){ sum(x) - 1 })
	result <- data.frame(
		STP_id, 
		as.numeric(input_load_local_g_d),
		as.numeric(load_local_g_d), 
		as.numeric(load_cumulated_g_d), 
		as.numeric(inhabitants_cumulated), 
		as.numeric(STP_count_cumulated), 
		row.names = NULL)
	names(result) <- c("STP_ID", "input_load_local_g_d", "load_local_g_d", "load_cumulated_g_d", "inhabitants_cumulated", "STP_count_cumulated")
	return(result)
	
}














