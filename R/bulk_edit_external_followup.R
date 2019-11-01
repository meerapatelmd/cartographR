#' Bulk flag rows for external followup
#' @param row_numbers single or range of row_numbers in PROGRESS_LIST$PARKED_FOR_EXTERNAL_FOLLOWUP
#' @import dplyr
#' @import mirroR
#' @import somersaulteR
#' @import typewriteR
#' @export

bulk_edit_external_followup <-
        function(progress_list, row_numbers, followup_request_edit = NULL, followup_question_edit = NULL) {
                if (nrow(progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP) > 0) {
                        dataframe <- progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP
                        
                        x_00 <- dataframe[row_numbers,]
                        
                        if (!is.null(followup_request_edit)) {
                                x_01 <- x_00 %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "x") %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = followup_request_edit) %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = bulk_followup_question) %>%
                                        dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                                        somersaulteR::call_mr_clean()
                        } else {
                                x_01 <- x_00 %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "x") %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = followup_request_edit) %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = bulk_followup_question) %>%
                                        #dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                                        somersaulteR::call_mr_clean()
                        }
                        
                        if (!is.null(followup_question_edit)) {
                                x_02 <- x_01 %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "x") %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = followup_request_edit) %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = bulk_followup_question) %>%
                                        dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                                        somersaulteR::call_mr_clean()
                        } else {
                                x_02 <- x_01 %>%
                                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "x") %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = followup_request_edit) %>%
                                        #dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = bulk_followup_question) %>%
                                        #dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                                        somersaulteR::call_mr_clean()
                        }
                        
                        
                        progress_list$STAGED <- dplyr::bind_rows(progress_list$STAGED,
                                                                 x_02)
                        
                        progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP <- progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP[-(row_numbers),]
                        
                        return(progress_list)
                        
                } else {
                        typewriteR::tell_me("ERROR: the PARKED_FOR_EXTERNAL_FOLLOWUP is empty.")
                        cat("\n\n")
                        typewriteR::stop_and_enter()
                        return(progress_list)
                }
        }