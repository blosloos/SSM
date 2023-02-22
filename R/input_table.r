#' Example input table containing sewage treatment plants and lakes
#'
#' @description This table is mainly intended as an example `input_table` to be used with function [wrap_table()]. 
#' It contains information on sewage treatment plants (STPs) from Switzerland and neighbouring countries, as well as swiss lakes.
#'
#'
#' @usage See function [wrap_table()].
#'
#'
#' @format An object of class data.frame with 683 rows and 29 columns, of which only some or all of the following columns are essential for usage with function [wrap_table()]:
#'* `ID`: unique IDs (numbers or characters) of river network nodes such as STPs and lakes.
#'* `inhabitants`: number of inhabitants which are treated by an individual STP node (0 or NA for lakes).
#'* `ID_next`: the ID of the next node directly downstream of each node (if any, else set entry to NA).
#'* columns `Q347_L_s_min` and `Q347_L_s_max`: minimum and maximum discharge just downstream of each STP or a lake outlet `[l / s]`.
#'
#' The following columns are required if `compound_elimination_method` in [wrap_table()] is set to `"compound_specific"`:
#'* columns `nitrification`, `denitrification` and `P_elimination`: `"TRUE"` or `"FALSE"` for STP nodes, and `"none"` for lakes or similar.
#'* `type_advanced_treatment`: any of `"GAC"`, `"combi"`, `"ozonation"`, `"PAC"` or `"undefined"`, `"redirection"`, or an empty entry if none of this applies for lakes 
#' or STPs without advanced treatment. Here, `"undefined"` is a placeholder for yet unspecified but scheduled advanced treatment; 
#' `"redirection"` implies that an STP will be closed and its influent rerouted to another STP at some point in time.
#' 
#'* `starting_year_advanced_treatment`: four-digit integer to state the year from which onwards the `type_advanced_treatment` applies (including the redirection to another STP).
#'* `redirecting_STP_target_STP_ID`: for any STP that contains `"redirection"` in `type_advanced_treatment`, the ID of another STP to which the discharge of its inhabitants 
#' is diverted.
#'
#' The following columns are required if `compound_elimination_method` in [wrap_table()] is set to `"node_specific"`:
#'* columns `STP_elimination_min` and `STP_elimination_max`: minimum and maximum elimination fractions `[0, 1]` for each STP node. Set elements to NA (or 0) for lakes.
#'
#' The following columns are required if `with_lake_elimination` in [wrap_table()] is set to `TRUE`:
#'* columns `lake_elimination_min` and `lake_elimination_max`: minimum and maximum elimination fractions `[0, 1]` for each lake node. Set elements to NA (or 0) for STPs.
#'
#'
#' The following column is required if `add_absolute_load` in [wrap_table()] is set to `TRUE`:
#'* `additional_absolut_load`: absolute compounds loads to be added at each node `[g / d]`.
#'
#'
#' @note Beware of the different time units used for columns `Q347_L_s_min/Q347_L_s_max` (seconds) vs. `additional_absolut_load` (days).
#'
#'
#' @author Rebekka Gulde \email{rebekka.gulde@vsa.ch}
#'
#'
"input_table"
