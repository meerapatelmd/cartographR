#' Bulk flag rows for external followup
#' @param row_numbers single or range of row_numbers in PROGRESS_LIST$QUEUE
#' @import dplyr
#' @import mirroR
#' @import somersaulteR
#' @import typewriteR
#' @importFrom crayon red
#' @export

bulk_flag_rows_for_external_followup <-
        function(progress_list, row_numbers, bulk_followup_request = "", bulk_followup_question = "") {
                if (nrow(progress_list$QUEUE) > 0) {
                        dataframe <- progress_list$QUEUE
                        
                        x <- dataframe[row_numbers,]
                        x <- x %>%
                                dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "x") %>%
                                dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = bulk_followup_request) %>%
                                dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = bulk_followup_question) %>%
                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                                somersaulteR::call_mr_clean()
                        
                        progress_list$STAGED <- dplyr::bind_rows(progress_list$STAGED,
                                                                 x)
                        
                        progress_list$QUEUE <- progress_list$QUEUE[-(row_numbers),]
                        
                        return(progress_list)
                        
                } else {
                        typewriteR::tell_me(crayon::red("Message: the QUEUE is empty."))
                        cat("\n\n")
                        typewriteR::stop_and_enter()
                        return(progress_list)
                }
        }
