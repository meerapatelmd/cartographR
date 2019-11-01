#' Bulk flags rows for denovo cui based on row numbers
#' @import dplyr
#' @import somersaulteR
#' @import mirroR
#' @import typewriteR
#' @importFrom crayon red
#' @export

flag_rows_for_denovo_cui <-
        function(progress_list, row_numbers, bulk_denovo_cui_justification = "") {
                if (nrow(progress_list$QUEUE) > 0) {
                dataframe <- progress_list$QUEUE
                x <- dataframe[row_numbers,]
                x <- x %>%
                        dplyr::mutate(KEY_DENOVO_CUI_JUSTIFICATION = bulk_denovo_cui_justification) %>%
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