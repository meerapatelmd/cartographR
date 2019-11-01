#' Get tokens for the valueset object outputed by get_valueset_for_target_vars
#' @param valueset_output output from get_valueset_for_target_vars
#' @export
#' 
get_tokens_for_target_vars <-
        function(valueset_output) {
                output <- list()
                for (i in 1:length(valueset_output)) {
                        output[[i]] <- cartographR::get_tokens_from_valueset(valueset_output[[i]])
                }
                names(output) <- names(valueset_output)
                return(output)
        }

