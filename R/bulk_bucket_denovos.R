#' Bulk flags rows for denovo cui based on row numbers
#' @param denovo_row_numbers denovo_row_numbers of progress_list$PARKED_FOR_DENOVO
#' @param bucket bucket name for downstream workflow: radiology, experimental drugs, precoordinated (combination of multiple existing concepts), qualifier 
#' @import dplyr
#' @import somersaulteR
#' @import mirroR
#' @import typewriteR
#' @importFrom crayon red
#' @export

bulk_bucket_denovos <-
        function(progress_list, denovo_row_numbers, bucket) {
                if (nrow(progress_list$PARKED_FOR_DENOVO) > 0) {
                dataframe <- progress_list$PARKED_FOR_DENOVO
                x <- dataframe[denovo_row_numbers,]
                x <- x %>%
                        dplyr::mutate(KEY_DENOVO_CUI_JUSTIFICATION = bulk_denovo_cui_justification) %>%
                        dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp()) %>%
                        dplyr::mutate(DENOVO_BUCKET = bucket)
                        somersaulteR::call_mr_clean()
                
                if (!(exists("DENOVO_CUI", envir = globalenv()))) {
                        y <- dplyr::bind_rows(progress_list) %>%
                                dplyr::mutate(DENOVO_BUCKET = "")
                        y <- y[-(1:nrow(x)),]
                        DENOVO_CUI <- dplyr::bind_rows(y,
                                                       x)
                        assign("DENOVO_CUI", DENOVO_CUI, envir = globalenv())
                } else {
                        DENOVO_CUI <- dplyr::bind_rows(DENOVO_CUI,
                                                       x)
                        assign("DENOVO_CUI", DENOVO_CUI, envir = globalenv())
                }
                
                progress_list$PARKED_FOR_DENOVO <- progress_list$PARKED_FOR_DENOVO[-(denovo_row_numbers),]
                assign("PROGRESS_LIST", progress_list, envir = globalenv())
                } else {
                        typewriteR::tell_me(crayon::red("Message: PARKED_FOR_DENOVO_CUI is empty."))
                        cat("\n\n")
                        typewriteR::stop_and_enter()
                        
                }
        }