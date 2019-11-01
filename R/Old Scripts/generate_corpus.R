#' Generate the final corpus of words for the data
#' @param valueset_output output from get_valueset_function
#' @param token_output output from get_tokens function
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @import somersaulteR
#' @importFrom caterpillaR delete_phrase_from_string
#' @export
#' 
generate_corpus <-
        function(valueset_output,
                 token_output) {
                input <- c(valueset_output,
                           token_output)
                
                input <- unlist(input)
                
                output <- data.frame(
                        CORPUS_LABEL = names(input),
                        CORPUS_VALUE = input
                ) %>%
                        somersaulteR::call_mr_clean() %>%
                        tidyr::separate(CORPUS_LABEL, into = c("NATIVE_TARGET_VARIABLE", "NATIVE_ID_VARIABLE", "CORPUS_SUB_LABEL"), 
                                        sep = "[.]{1}", remove = FALSE)
                
                output$CORPUS_SUB_LABEL <-  mapply(caterpillaR::delete_phrase_from_string, output$CORPUS_SUB_LABEL, output$NATIVE_TARGET_VARIABLE)
                
                output <-
                        output %>%
                        tidyr::separate(col = CORPUS_SUB_LABEL, into = c("BLANK_01", "CORPUS_VALUESET_LABEL", "CORPUS_TOKEN_LABEL"),
                                 sep = "_", remove = TRUE)
                
                output$BLANK_01 <- NULL
                
                output <-
                        output %>%
                        dplyr::mutate(CORPUS_VALUESET_NUMBER = str_remove_all(CORPUS_VALUESET_LABEL, "[^0-9]")) %>%
                        dplyr::mutate(CORPUS_TOKEN_NUMBER = str_remove_all(CORPUS_TOKEN_LABEL, "[^0-9]")) %>%
                        somersaulteR::mutate_all_na_to_blank() %>%
                        dplyr::mutate_at(vars(CORPUS_VALUESET_NUMBER, CORPUS_TOKEN_NUMBER), list(~ifelse(. == "", "0", .))) %>%
                        dplyr::mutate_at(vars(CORPUS_VALUESET_NUMBER, CORPUS_TOKEN_NUMBER), list(~as.integer(.)+1))

                
                output <-
                left_join(output,
                          output %>%
                                  dplyr::group_by(CORPUS_VALUE) %>%
                                  dplyr::summarize(CORPUS_VALUE_FREQUENCY = length(CORPUS_VALUE)) %>%
                                  dplyr::ungroup()) %>%
                        dplyr::arrange(desc(CORPUS_VALUE_FREQUENCY)) %>%
                        tidyr::unite(CORPUS_TYPE, CORPUS_VALUESET_LABEL, CORPUS_TOKEN_LABEL, sep = " ", remove = FALSE) %>%
                        somersaulteR::call_mr_clean() %>%
                        dplyr::mutate(CORPUS_TYPE = stringr::str_remove_all(CORPUS_TYPE, "[0-9]"))
                
                return(output)
                       
        }