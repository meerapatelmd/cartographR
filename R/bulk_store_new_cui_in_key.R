#' Appends local key with new concept annotation using a dataframe input instead of a single line
#' @param dataframe dataframe with columns IDENTITY_ID, KEY_FIELD, and KEY_CONCEPT_NAME
#' @import somersaulteR
#' @import mirroR
#' @import readr
#' @import dplyr
#' @export

bulk_store_new_cui_in_local_key <-
        function(path_to_key_csv,
                 dataframe,
                 key_cui = "",
                 key_cui_mrconso_str = "",
                 key_cui_justification = "",
                 key_cui_flag_for_review = "",
                 key_denovo_cui_justification = "",
                 key_denovo_cui_suggested_definition = "",
                 key_denovo_cui_assigned = "",
                 key_external_followup_flag = "",
                 key_external_followup_request = "",
                 key_external_followup_question = "",
                 key_external_followup_timestamp = "") {
                
                dataframe <- dataframe %>%
                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp(),
                                        KEY_CUI = key_cui,
                                        KEY_CUI_MRCONSO_STR = key_cui_mrconso_str,
                                        KEY_CUI_JUSTIFICATION = key_cui_justification,
                                        KEY_CUI_FLAG_FOR_REVIEW = key_cui_flag_for_review,
                                        KEY_DENOVO_CUI_JUSTIFICATION = key_denovo_cui_justification,
                                        KEY_DENOVO_CUI_SUGGESTED_DEFINITION = key_denovo_cui_suggested_definition,
                                        KEY_DENOVO_CUI_ASSIGNED = key_denovo_cui_assigned,
                                        KEY_EXTERNAL_FOLLOWUP_FLAG = key_external_followup_flag,
                                        KEY_EXTERNAL_FOLLOWUP_REQUEST = key_external_followup_request,
                                        KEY_EXTERNAL_FOLLOWUP_QUESTION = key_external_followup_question,
                                        KEY_EXTERNAL_FOLLOWUP_TIMESTAMP = key_external_followup_timestamp) %>%
                        somersaulteR::call_mr_clean()
                
                        historical_key <- readr::read_csv(path_to_key_csv, col_types = cols(.default = "c"))
                        
                        new_key <-
                                dplyr::bind_rows(historical_key,
                                          dataframe) %>%
                                somersaulteR::mutate_all_na_to_blank()
                        
                        readr::write_csv(new_key, path_to_key_csv)
        }

