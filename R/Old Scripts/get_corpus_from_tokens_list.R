#' Creates a corpus from the output of get_tokens_from_valueset()
#' Corpus: all the unique tokens belonging to the field in question
#' @param tokens_list output from get_tokens_from_valueset
#' @export

get_corpus_from_tokens_list <-
        function(tokens_list) {
                data.frame(
                        corpus = unlist(tokens_list)) %>%
                        dplyr::group_by(corpus) %>%
                        dplyr::summarise(frequency = length(corpus)) 
        }