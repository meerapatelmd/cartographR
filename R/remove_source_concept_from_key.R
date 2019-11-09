#' Completely removes all traces of a concept that didn't need to be mapped from the key
#' @param identity_id identity_id to which the concept belongs
#' @param key_field the field this concept belongs to
#' @param key_concept_name the exact name of the concept
#' @return An updated KEY that is written at its existing file destination that has removed all record of the given source concept
#' @import dplyr
#' @import readr
#' @import typewriteR
#' @export


remove_source_concept_from_key <-
        function(identity_id, 
                 key_field,
                 key_concept_name) {
                
                KEY_00 <- readr::read_csv("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/KEY/REDCap/KEY_REDCAP_TO_UMLS.csv",
                                           col_types = cols(.default = "c"))
                
                remove_from_key_df <-
                        KEY_00 %>%
                        dplyr::filter(IDENTITY_ID == identity_id,
                                      KEY_FIELD == key_field,
                                      KEY_CONCEPT_NAME == key_concept_name)
                
                NEW_KEY <-
                        dplyr::setdiff(KEY_00, remove_from_key_df)
                
                typewriteR::tell_me("There are", nrow(NEW_KEY), "rows in the updated key while there are", nrow(KEY_00), "in the current key. Overwrite?")
                typewriteR::stop_and_enter()
                
                readr::write_csv(NEW_KEY, path = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/KEY/REDCap/KEY_REDCAP_TO_UMLS.csv")
        }
