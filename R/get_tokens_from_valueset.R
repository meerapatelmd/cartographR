#' Get list of tokens for a list of valuesets
#' Tokens: all unique lowercase, stripped of punctuation, split at single spaces words
#' @param valueset_list output from get_valueset()
#' @importFrom stringr str_remove_all
#' @export
#' 
get_tokens_from_valueset <-
        function(valueset_list) {
                output <- list()
                for (i in 1:length(valueset_list)) {
                        valueset <- valueset_list[[i]]
                        output[[i]] <- unique(stringr::str_remove_all(tolower(unlist(strsplit(paste(valueset), split = " "))), "[[:punct:]]"))
                }
                return(output)
        }