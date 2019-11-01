#' Get list of tokens for a list of valuesets
#' Tokens: all unique lowercase, split at single spaces words
#' @param valueset_list output from get_valueset()
#' @importFrom stringr str_remove_all
#' @export
#' 
get_tokens_from_valueset <-
        function(valueset_list) {
                output <- list()
                for (i in 1:length(valueset_list)) {
                        valueset <- valueset_list[[i]]
                        valueset_names <- names(valueset)
                        
                        output[[i]] <- unique(unlist(valueset))
                        names(output[[i]]) <- paste0(valueset_names, "_TOKENS")
                        
                        output[[i]] <- unlist(strsplit(output[[i]], split = " "))
                        output[[i]] <- output[[i]][!(duplicated(output[[i]]))]
                        
                }
                names(output) <- names(valueset_list)
                return(output)
        }