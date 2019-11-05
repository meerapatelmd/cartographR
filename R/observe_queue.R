#' Observe the PROGRES_LIST$QUEUE to map
#' @import dplyr
#' @import typewriteR
#' @export

observe_queue <-
        function(progress_list) {
                progress_list$QUEUE %>%
                        dplyr::select(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                        typewriteR::create_menu()
        }