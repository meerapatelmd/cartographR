#' Adds PROGRESS_LIST$Staged to KEY
#' @import dplyr
#' @import readr
#' @import somersaulteR
#' @export

progress_list_staged_to_key <-
        function(progress_list, path_to_key_csv) {
                CURRENT_KEY <- key_from_csv(path_to_key_csv)
                ADD_TO_KEY <- progress_list$STAGED 
                UPDATED_KEY <- dplyr::bind_rows(CURRENT_KEY %>%
                                                        somersaulteR::call_mr_clean(),
                                                ADD_TO_KEY)
                readr::write_csv(UPDATED_KEY, path = path_to_key_csv)
        }