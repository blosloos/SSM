#' Example input table containing sewage treatment plants and lakes
#'
#' @description This table is mainly intended as an example `input_table` to be used with function [wrap_table()]. 
#' It contains information on sewage treatment plants (STPs) from Switzerland and neighbouring countries, as well as swiss lakes.
#'
#'
#' @usage See function [wrap_table()].
#'
#'
#' @format An object of class data.frame with 683 rows and 29 columns, of which the following columns are essential to be used with function [wrap_table()]:
#'* ID
#'* inhabitants
#'* ID_next
#'* Q347_L_s_min
#'* Q347_L_s_max
#'
#' The follwing columns are required if `` in [wrap_table()] is set to ``:
#'* nitrification
#'* denitrification
#'* P_elimination
#'* type_advanced_treatment
#'* starting_year_advanced_treatment
#'* redirecting_STP_target_STP_ID
#'
#' The follwing columns are required if `` in [wrap_table()] is set to ``:
#'* lake_elimination_min
#'* lake_elimination_max
#'
#' The follwing columns are required if `` in [wrap_table()] is set to ``:
#'* STP_elimination_min
#'* STP_elimination_max
#'*
#' The follwing column is required if `` in [wrap_table()] is set to ``:
#'* additional_absolut_load
#'
#' @author Rebekka Gulde \email{rebekka.gulde@vsa.ch}
#'
#'
"input_table"
