#' Observe the PROGRES_LIST$PARKED_FOR_DENOVO to bucket these concepts
#' @import dplyr
#' @import typewriteR
#' @export

observe_parked_for_denovo_to_bucket <-
        function(progress_list) {
                progress_list$PARKED_FOR_DENOVO %>%
                        dplyr::select(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME, KEY_DENOVO_CUI_JUSTIFICATION) %>%
                        typewriteR::create_menu()
        }