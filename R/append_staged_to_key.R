#' Adds PROGRESS_LIST$Staged to KEY
#' @import dplyr
#' @import readr
#' @import somersaulteR
#' @export

append_staged_to_key <-
        function(progress_list, path_to_key_csv) {
                KEY <- key_from_csv(path_to_key_csv)
                CURRENT_KEY <- KEY
                ADD_TO_KEY <- PROGRESS_LIST$STAGED 
                UPDATED_KEY <- dplyr::bind_rows(CURRENT_KEY %>%
                                                        somersaulteR::call_mr_clean(),
                                                ADD_TO_KEY)
                readr::write_csv(UPDATED_KEY, path = path_to_key_csv)
        }