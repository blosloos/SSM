#' Calculate topology matrix for STP and lake nodes in a river network.
#'
#' @param ID Character or integer vector containing IDs of nodes.
#' @param ID_next Character or integer  vector of length equal to ID, containing the ID of the next node directly downstream of each node (if any, else set entry to NA).
#' @param NA_next_ignore Logical. Set entries in vector ID_next which are not found in vector ID to NA? Else, throw an error (the default, FALSE).
#' @param insert_id_in_topo_matrix Logical, default FALSE. Insert IDs into returned matrix.
#' @param only_direct_upstream Logical, default FALSE. Return only STPs/lakes directly upstream?
#'
#' @description Calculate a topology matrix to mark all sewage treatment plants (STPs) or lakes (or similar network nodes) upstream 
#' of an individual STP/lake in a river network. Using only_direct_upstream, these can either be all nodes upstream, or only those in direct
#' upstream adjacency. Mainly to be used in function calc_load().
#'
#' @returns A matrix with dimensions and column/row names equal to argument ID. Columns contain 1 
#' (or IDs for insert_id_in_topo_matrix) to denote upstream nodes of each STP/lake node, and 0 otherwise.
#'
#' @seealso {[calc_load()]}
#'
#' @examples
#' ID <- c(1, 2, 3, 4, 5)
#' ID_next <- c(4, 3, 4, 5, NA) # node with ID = 5 has no downstream node (hence ID_next = NA), but all nodes discharge to it (hence all its column set to 1):
#' make_topology(ID, ID_next)


make_topology <- function(
	ID = FALSE,	
	ID_next, 						# NA if none available				
	NA_next_ignore = FALSE,			# ara_id_next not in ID? -> set to NA as well
	insert_id_in_topo_matrix = FALSE,
	only_direct_upstream = FALSE
){

	###############################################
	# check inputs & defaults 
	ID <- as.character(ID)
	ID_next <- as.character(ID_next)
	len <- length(ID_next)
	if(is.logical(ID[1]) & !isTRUE(ID[1])) ID <- seq(len) else{ 
		if(length(ID) != len) stop("Problem in make_topology: ID_next and ID must be of same length") 
		if(any(is.na(ID))) stop("Problem in make_topology: ID must not contain NAs") 
	}
	if(NA_next_ignore){
		if(any(!(ID_next[!is.na(ID_next)] %in% ID))) stop("Problem in make_topology: ID and ID_next mismatching")
	}else ID_next[!(ID_next %in% ID)] <- NA
	###############################################
	bin_link_matrix <- matrix(nrow = len, ncol = len, 0)
	STP_nr_next <- match(ID_next, ID)					# NAs returned
	for(i in 1:len) bin_link_matrix[i, STP_nr_next[i]] <- 1 	# NAs skipped
	if(!only_direct_upstream) topo_matrix <- solve(diag(len) - bin_link_matrix)			
	colnames(topo_matrix) <- rownames(topo_matrix) <- ID
	if(isTRUE(insert_id_in_topo_matrix)) for(i in ncol(topo_matrix)) topo_matrix[, i] <- topo_matrix[, i] * as.numeric(ID) # inserts where 1
	###############################################
	return(topo_matrix) # columns mark upstream STPs of each STP
	
}

