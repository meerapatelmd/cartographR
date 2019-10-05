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
                        output[[i]] <- unique(unlist(strsplit(paste(valueset), split = " ")))
                        names(output[[i]]) <- paste0(names(output[[i]]), "_TOKENS")
                }
                names(output) <- names(valueset_list)
                return(output)
        }