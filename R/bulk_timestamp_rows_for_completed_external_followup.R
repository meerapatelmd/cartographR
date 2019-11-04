#' Bulk changes PROGRESS_LIST$PARKED_FOR_EXTERNAL_FOLLOWUP and stages those changes
#' @param progress_list full PROGRESS_LIST object
#' @param row_numbers row_numbers of PROGRESS_LIST$PARKED_FOR_EXTERNAL_FOLLOWUP
#' @import dplyr
#' @import mirroR
#' @import typewriteR
#' @export

bulk_timestamp_rows_for_completed_external_followup <-
        function (progress_list, row_numbers) {
                
        if (nrow(progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP) > 0) {
                dataframe <- progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP
                x <- dataframe[row_numbers, ]
                x <- x %>% dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = "") %>% 
                        dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_TIMESTAMP = mirroR::get_timestamp())
                progress_list$QUEUE <- dplyr::bind_rows(progress_list$QUEUE, 
                                                         x)
                progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP <- progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP[-(row_numbers),]
                return(progress_list)
        }
        else {
                typewriteR::tell_me(crayon::red("Message: PARKED_FOR_EXTERNAL_FOLLOWUP is empty."))
                cat("\n\n")
                typewriteR::stop_and_enter()
                return(progress_list)
        }
}