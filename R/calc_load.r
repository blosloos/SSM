#' Calculate local and cumulated loads of a compound in a river network
#'
#' @inheritParams make_topology
#' @param inhabitants Integer vector of length equal to ID. Number of inhabitants which are treated by an individual STP node (0 or NA for lakes).
#' @param compound_load_gramm_per_capita_and_day Single numeric value. Amount of the compound excreted per person and day `[g / d]`.
#' @param compound_elimination_method Strings `"compound_specific"` (the default) or `"node_specific"` on how to calculate compound elimination, cp. Details.
#' @param compound_elimination_STP Required for compound_elimination_method `"compound_specific"`. 
#' Single-row dataframe with elimination fractions `[0, 1]` for the following STP treatment steps aka column names:
#'* COD_treatment
#'* nitrification
#'* denitrification
#'* P_elimination
#'* GAC
#'* combi
#'* ozonation
#'* PAC
#'* undefined
#'
#' @param STP_treatment_steps Required for compound_elimination_method `"compound_specific"`. A dataframe of strings with number of rows equal to length of vector ID,
#' and with four named columns indicating the following treatment steps:
#'* nitrification, denitrification and P_elimination: `"TRUE"` or `"FALSE"` for STP nodes, and `"none"` for lakes or similar.
#'* type_advanced_treatment: any one of `"GAC"`, `"combi"`, `"ozonation"`, `"undefined"`, `"undefined"` or `"redirection"`, or any other (empty) string for lake nodes.
#'
#' @param STP_elimination_rates Required for compound_elimination_method `"node_specific"`. 
#' Numeric vector of length equal to ID, containing elimination fractions `[0, 1]` for each STP node. Set elements to NA (or 0) for lakes.
#' @param with_lake_elimination Logical. Include elimination by lakes? Defaults to TRUE.
#' @param lake_eliminination_rates Numeric vector of length equal equal to ID, containing elimination fractions `[0, 1]` for each lake node. 
#' Set elements to NA (or 0) for STPs. Only used when with_lake_elimination is set to TRUE.  
#' @param add_absolute_load Logical, with default FALSE. Add further absolute loads at each node, and in additional to the ones calculated 
#' via compound_load_gramm_per_capita_and_day plus compound_elimination_method?
#' @param absolute_loads Numeric vector of length equal to ID with absolute loads to be added `[g / d]`. Used if add_absolute_load set to TRUE.
#' @param return_data Strings `"loads"` (the default) or `"matrix"` to return either the calculated loads or the underlying node routing matrix.
#'
#' @description Based on discharge via sewage treatment plantes (STPs) and the elimination rates in STPs
#' and lakes, this function estimates the locally released and cumulated loads of a compound along a river network. 
#' 
#'
#' @returns Depending on argument `return_data`, either the routing matrix (cp. [make_topology()]) or a dataframe with the following named columns:
#' 
#'* `ID`: Node IDs of STPs and lakes
#'* `input_load_local_g_d`: compound amount released into each STP `[g / d]`, i.e., inhabitants * compound_load_gramm_per_capita_and_day 
#'* `load_local_g_d`: compound amount discharged from each STP after elimination `[g / d]`
#'* `load_cumulated_g_d`: cumulated compound amount just downstream of each STP or lake `[g / d]`
#'* `inhabitants_cumulated`: cumulated number of inhabitants just downstream of each STP or lake
#'* `STP_count_cumulated`: cumulated number of STPs just downstream of each STP or lake
#'
#'
#' @details This function estimates the aquatic loads of a single compound in a river network in several steps: 
#'
#' Firstly, the local input into each STP is derived from its connected number of inhabitants multiplied by a mean value of consumption for this compound.
#' (cp. returned `input_load_local_g_d`).
#'
#' Secondly, fractions of this amount are either eliminated by a single STP-specific elimination rate (compound_elimination_method = `"node_specific"`) or
#' based on the treatment steps of each STP in combination with treatment-specific elimination rates of the compound (compound_elimination_method = `"compound_specific"`).
#'
#' The loads thus released from each STP (cp. returned `load_local_g_d`) are then cumulated through the river network in a third step (cp. returned `load_cumulated_g_d`), 
#' and with a routing based on `ID_next`.
#'
#' Optionally, and in a fourth stage, these cumulated loads are further degraded in lakes (arguments `with_lake_elimination` and `lake_eliminination_rates`) 
#' or can receive additional absolut inputs via `arguments add_absolute_load` and `absolute_loads`. 
#' For the lake elimination, all loads of upstream STPs are first summed and then reduced by the lake-specific `lake_eliminination_rates`.
#'
#' The `"compound_specific"` elimination rates are set as a product, and as far as they are relevant at each STP based on input `STP_treatment_steps`.
#' If a treatment step is not present at an STP, the concerned entry in `compound_elimination_STP` is not multiplied into this product, except for   
#' the nitrification step. If the latter is absent at an STP, compound_elimination_STP$COD_treatment is used as a factor instead. 
#' 
#' 
#' @note There is no explicit indication as to whether an entry in vector ID represents an STP or a lake or something else. 
#' Thus, the user has to take care (depending on function arguments compound_elimination_method and with_lake_elimination) that no entries other 
#' than `"none"` or NA exist for lakes in inputs STP_treatment_steps or STP_elimination_rates, respectively; 
#' and in turn as NAs in input lake_eliminination_rates for the STP nodes.
#'
#'
#' @seealso [wrap_table()]
#'
#' @examples Bla.
#'


calc_load <- function(
	ID,
	ID_next,
	inhabitants,	
	compound_load_gramm_per_capita_and_day,		
	compound_elimination_method = "compound_specific",
	compound_elimination_STP = NULL,
	STP_treatment_steps = NULL,
	STP_elimination_rates = FALSE,
	with_lake_elimination = TRUE,
	lake_eliminination_rates = NULL,
	add_absolute_load = FALSE,
	absolute_loads = FALSE,
	return_data = "loads"
	
){

	###############################################
	# check inputs & defaults
	if(length(compound_load_gramm_per_capita_and_day) > 1) stop("Problem in calc_load: compound_load_gramm_per_capita_and_day should consist of one value only")
	if(!identical(length(ID), length(inhabitants))) stop("Problem in calc_load: length of vectro inhabitants not equal to the ID vector.")
	if(!is.numeric(inhabitants)) stop("Problem in calc_load: vector inhabitants must be numeric.")
	if(any(is.na(inhabitants))) inhabitants[is.na(inhabitants)] <- 0
	if(!is.data.frame(compound_elimination_STP)) stop("Problem in calc_load, argument compound_elimination_STP is not a dataframe.")
	if(any(!sapply(compound_elimination_STP, is.numeric))) stop("Problem in calc_load, dataframe compound_elimination_STP has non-numeric entries.")
	STP_steps <- c("COD_treatment", "nitrification", "denitrification", "P_elimination", "GAC", "combi", "ozonation", "PAC", "undefined")
	not_found <- !(names(compound_elimination_STP) %in% STP_steps)
	if(any(not_found)) stop(paste0("Problem in calc_load, argument compound_elimination_STP: entry ", paste(names(compound_elimination_STP)[not_found], collapse = ", "), " is missing."))
	if(any((compound_elimination_STP < 0) & (compound_elimination_STP > 1))) stop("Problem in calc_load: compound_elimination_STP not within [0,1]")
	if(!(compound_elimination_method %in% c("compound_specific", "node_specific"))) stop("Problem in calc_load: invalid compound_elimination_method, must be either compound_specific or STP individual.")
	if(compound_elimination_method == "node_specific"){ 
		if(STP_elimination_rates[1] == FALSE) stop("Problem in calc_load: compound_elimination_method set to node_specific, but no STP_elimination_rates provided. Please revise.")
		if(length(STP_elimination_rates) != length(ID)) stop("Problem in calc_load: for node_specific compound_elimination_method, length of STP_elimination_rates must equal that of ID vector.") 
		STP_elimination_rates[is.na(as.numeric(STP_elimination_rates))] <- 0
	}
	if(compound_elimination_method == "compound_specific"){ 
		if(nrow(STP_treatment_steps) != length(ID)) stop("Problem in calc_load: number of rows for STP_treatment_steps not equal to length of ID") 
		if(!all(names(STP_treatment_steps) %in% c("nitrification", "denitrification", "P_elimination", "type_advanced_treatment", "starting_year_advanced_treatment"))){
			# ... with starting_year_advanced_treatment used in wrap_table
			stop("Problem in calc_load: missing or invalid column names for STP_treatment_steps")
		}
	}
	if(with_lake_elimination) lake_eliminination_rates[is.na(as.numeric(lake_eliminination_rates))] <- 0
	that_not <- which(!(ID_next[!is.na(ID_next)] %in% ID))
	if(length(that_not)) stop(paste0("Problem in calc_load: invalid ID_next entry detected: ", paste(ID_next[!is.na(ID_next)][that_not], collapse = ", ")))
	###############################################
 
	###############################################	
	# calculate elimiation rates
	compound_elimination_STP_calc <- rep(0, length(ID))
	for(i in 1:length(ID)){

		#######################################
		if(compound_elimination_method == "compound_specific"){
			if(any(STP_treatment_steps[i, c("nitrification", "denitrification", "P_elimination")] == "none")){ # for non-STP nodes
				if(!all(STP_treatment_steps[i, c("nitrification", "denitrification", "P_elimination")] == "none")) stop(
					paste0("Problem in calc_load: not all entries in STP_treatment_steps for columns nitrification, denitrification and P_elimination set to none for lake with ID ", ID[i])
				)
				compound_elimination_STP_calc[i] <- 0
			}else{	# for STPs
				compound_elimination_STP_calc[i] <- prod(1 - c(								
					if(STP_treatment_steps[i, "nitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$nitrification else compound_elimination_STP$COD_treatment,
					if(STP_treatment_steps[i, "denitrification"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$denitrification,
					if(STP_treatment_steps[i, "P_elimination"] %in% c("yes", "Yes", "ja", "Ja", "TRUE")) compound_elimination_STP$P_elimination,		
					if(!is.na(STP_treatment_steps[i, "type_advanced_treatment"])){
						compound_elimination_STP[
							names(compound_elimination_STP) == STP_treatment_steps[i, "type_advanced_treatment"]
						][[1]]
					}else 0
				))		
			}
			if(is.na(compound_elimination_STP_calc[i])) stop("Problem in calc_load: NA for compound_elimination_STP_calc detected.")
		}
		#######################################
		if(compound_elimination_method == "node_specific"){ 
			if(!length(STP_elimination_rates[i])) next
			if(is.na(STP_elimination_rates[i])) next
			if(is.na(as.numeric(STP_elimination_rates[i]))) next
			compound_elimination_STP_calc[i] <- (1 - as.numeric(STP_elimination_rates[i]))
		}
		#######################################
		
	}
	###############################################			

	###############################################
	# run model: iterate thourhg STP/lake nodes
	input_load_local_g_d <- inhabitants * compound_load_gramm_per_capita_and_day 
	load_local_g_d <- inhabitants * compound_load_gramm_per_capita_and_day * compound_elimination_STP_calc
	load_local_g_d[is.na(load_local_g_d)] <- 0 # for lakes
	if(add_absolute_load){
		absolute_loads[is.na(absolute_loads)] <- 0
		load_local_g_d <- load_local_g_d + absolute_loads
	}
	if(!with_lake_elimination) lake_eliminination_rates <- rep(0, length(lake_eliminination_rates)) else{
		if(any(lake_eliminination_rates < 0) | any(lake_eliminination_rates > 1)) stop("Problem in calc_load: lake_eliminination_rate must be within [0, 1]")
		if(compound_elimination_method == "node_specific") if(any(lake_eliminination_rates != 0 & STP_elimination_rates != 0 )) stop("
			Problem in calc_load: lake_eliminination_rates and STP_elimination_rates must be exclusive: not both can eliminate together.")
	}
	load_cumulated_g_d <- load_local_g_d
	not_loop_endless <- 0 # a safety measure on finishing calculation within reasonable number of iterations
	ID_next_iter <- ID_next
	do_calc_node <- rep(TRUE, length(ID))
	topo_matrix <- matrix(nrow = length(ID), ncol = length(ID), 0)
	colnames(topo_matrix) <- rownames(topo_matrix) <- ID
	while(
		any(do_calc_node) &
		not_loop_endless < 1E5
	){		
		
		for(k in 1:length(ID)){ # check through STPs / lakes
			# (1) still required to be calculated?
			if(!do_calc_node[k]) next
			# (2) still awaiting load input, i.e., still another STP points to ID[k]?
			if(ID[k] %in% ID_next_iter) next		
			# (3) for lakes: reduce load before adding on
			load_cumulated_g_d[k] <- load_cumulated_g_d[k] * (1 - lake_eliminination_rates[k])		
			# (4) adding on load to next STP or lake
			if(!is.na(ID_next_iter[k])){
				load_cumulated_g_d[ID == ID_next_iter[k]] <- load_cumulated_g_d[ID == ID_next_iter[k]] + load_cumulated_g_d[k]
				topo_matrix[
					rownames(topo_matrix) == ID[k],
					colnames(topo_matrix) == ID_next_iter[k]
				] <- 1
				ID_next_iter[k] <- NA	# for point (2)
			}
			# (5) mark that node has been done
			do_calc_node[k] <- FALSE
		
		}
		not_loop_endless <- not_loop_endless + 1
			
	}
	if(not_loop_endless >= 1E5) stop("Problem in calc_load: load routing through STP and lake network did not finish within expected number of iterations (check ID_next for circular pointing).") 
	###############################################
	
	###############################################
	# return routing matrix
	len <- length(ID)
	topo_matrix <- solve(diag(len) - topo_matrix)
	if(!sum(topo_matrix)) stop("Problem in calc_load: no relations between STPs found. Is ID_next all set to 0?")	
	if(return_data == "matrix")	return(topo_matrix)
	###############################################	
	
	###############################################	
	# return loads
	inhabitants_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = inhabitants)
	STP_count_cumulated <- apply(topo_matrix, MARGIN = 2, function(x){sum(x) - 1 })
	result <- data.frame(
		ID, 
		as.numeric(input_load_local_g_d),
		as.numeric(load_local_g_d), 
		as.numeric(load_cumulated_g_d), 
		as.numeric(inhabitants_cumulated), 
		as.numeric(STP_count_cumulated), 
		row.names = NULL)
	names(result) <- c("ID", "input_load_local_g_d", "load_local_g_d", "load_cumulated_g_d", "inhabitants_cumulated", "STP_count_cumulated")
	return(result)
	###############################################
	
}














