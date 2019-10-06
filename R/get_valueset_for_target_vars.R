#' Get a list of value sets per target variable
#' @param id_var id variable that will tag the output
#' @param target_vars character vector of 1 or greater of target variable names
#' @export

get_valuesets_for_target_variables <-
        function(dataframe, id_var, target_vars) {
                output <- list()
                for (i in 1:length(target_vars)) {
                        output[[i]]        <- cartographR::get_valueset(dataframe, id_var, target_vars[i])
                }
                names(output) <- target_vars
                return(output)
        }
