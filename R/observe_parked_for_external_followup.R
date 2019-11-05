#' Observe the PROGRES_LIST$PARKED_FOR_EXTERNAL_FOLLOWUP to map
#' @import dplyr
#' @import typewriteR
#' @export

observe_parked_for_external_followup <-
        function(progress_list) {
                progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP %>%
                        dplyr::select(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME, dplyr::contains("EXTERNAL_FOLLOWUP")) %>%
                        typewriteR::create_menu()
        }