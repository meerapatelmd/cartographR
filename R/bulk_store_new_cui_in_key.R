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
                 key_cui = NA,
                 key_cui_mrconso_str = NA,
                 key_cui_justification = NA,
                 key_cui_flag_for_review = NA,
                 key_denovo_cui_justification = NA,
                 key_denovo_cui_suggested_definition = NA,
                 key_denovo_cui_assigned = NA,
                 key_external_followup_flag = NA,
                 key_external_followup_request = NA,
                 key_external_followup_question = NA,
                 key_external_followup_timestamp = NA) {
                
                dataframe_00 <- dataframe %>%
                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp())
                
                if (!is.na(key_cui)) {
                        dataframe_01 <-
                                dataframe_00 %>%
                                dplyr::mutate(KEY_CUI = key_cui)
                } else {
                        dataframe_01 <-
                                dataframe_00
                }
                
                if (!is.na(key_cui_mrconso_str)) {
                        dataframe_02 <-
                                dataframe_01 %>%
                                dplyr::mutate(KEY_CUI_MRCONSO_STR = key_cui_mrconso_str)
                } else {
                        dataframe_02 <-
                                dataframe_01
                }
                
                if (!is.na(key_cui_justification)) {
                        dataframe_03 <- dataframe_02 %>% dplyr::mutate(KEY_CUI_JUSTIFICATION = key_cui_justification)
                } else {
                        dataframe_03 <- dataframe_02
                }
                
                if (!is.na(key_cui_flag_for_review)) {
                        dataframe_04 <- dataframe_03 %>% dplyr::mutate(KEY_CUI_FLAG_FOR_REVIEW = key_cui_flag_for_review)
                } else {
                        dataframe_04 <- dataframe_03
                }
                
                if (!is.na(key_denovo_cui_justification)) {
                        dataframe_05 <- dataframe_04 %>% dplyr::mutate(KEY_DENOVO_CUI_JUSTIFICATION = key_denovo_cui_justification)
                } else {
                        dataframe_05 <- dataframe_04
                }
                
                if (!is.na(key_denovo_cui_suggested_definition)) {
                        dataframe_06 <- dataframe_05 %>% dplyr::mutate(KEY_DENOVO_CUI_SUGGESTED_DEFINITION = key_denovo_cui_suggested_definition)
                } else {
                        dataframe_06 <- dataframe_05
                }
                
                if (!is.na(key_denovo_cui_assigned)) {
                        dataframe_07 <- dataframe_06 %>% dplyr::mutate(KEY_DENOVO_CUI_ASSIGNED = key_denovo_cui_assigned)
                } else {
                        dataframe_07 <- dataframe_06
                }
                
                if (!is.na(key_external_followup_flag)) {
                        dataframe_08 <- dataframe_07 %>% dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_FLAG = key_external_followup_flag)
                } else {
                        dataframe_08 <- dataframe_07
                }
                
                if (!is.na(key_external_followup_request)) {
                        dataframe_09 <- dataframe_08 %>% dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_REQUEST = key_external_followup_request)
                } else {
                        dataframe_09 <- dataframe_08
                }
                
                if (!is.na(key_external_followup_question)) {
                        dataframe_10 <- dataframe_09 %>% dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_QUESTION = key_external_followup_question)
                } else {
                        dataframe_10 <- dataframe_09
                }
                
                if (!is.na(key_external_followup_timestamp)) {
                        dataframe_11 <- dataframe_10 %>% dplyr::mutate(KEY_EXTERNAL_FOLLOWUP_TIMESTAMP = key_external_followup_timestamp)
                } else {
                        dataframe_11 <- dataframe_10
                }
                
                dataframe_xx <-
                        dataframe_11 %>%
                        somersaulteR::call_mr_clean()
                
                historical_key <- readr::read_csv(path_to_key_csv, col_types = cols(.default = "c"))
                        
                new_key <-
                                dplyr::bind_rows(historical_key,
                                          dataframe_xx) %>%
                                somersaulteR::mutate_all_na_to_blank()
                        
                readr::write_csv(new_key, path_to_key_csv)
        }

