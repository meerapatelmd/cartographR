#' Looks up native attributes from IDENTITY
#' @param identity_id IDENTITY_ID
#' @param key_field name of target field
#' @param key_concept_name name of concept
#' @import readr
#' @import somersaulteR
#' @import dplyr
#' @export


lookup_native_attributes <-
        function(identity_id, key_field, key_concept_name) {
                IDENTITY_ID <- identity_id
                KEY_FIELD <- key_field
                KEY_CONCEPT_NAME <- key_concept_name
                
                TARGET_VAR <- enquo(KEY_FIELD)
                
                relationship_df <-
                                readr::read_csv("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/KEY/REDCap/RELATIONSHIP.csv", col_types = cols(.default = "c")) %>%
                                somersaulteR::call_mr_clean() %>%
                                dplyr::filter_at(vars(FIELD_AS_CONCEPT_01), any_vars(. == KEY_FIELD))
                
                # print(relationship_df)
                # typewriteR::stop_and_enter()
                
                x <-
                        readr::read_csv("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/KEY/IDENTITY.csv", col_types = cols(.default = "c")) %>%
                        dplyr::rename(KMI_PERMISSIBLE_VALUE_LABEL = PERMISSIBLE_VALUE_LABEL) %>%
                        somersaulteR::call_mr_clean() %>%
                        dplyr::select(IDENTITY_ID, unique(relationship_df$FIELD_AS_CONCEPT_02)) %>%
                        dplyr::filter_at(vars(IDENTITY_ID), all_vars(. == identity_id)) %>%
                        dplyr::filter_at(vars(!!TARGET_VAR), all_vars(. == key_concept_name))
                
                # print(x)
                # typewriteR::stop_and_enter()
                
                label_df <- dplyr::left_join(data.frame(FIELD_AS_CONCEPT_02 = names(x)),
                                 relationship_df)
                
                labels <- label_df %>%
                                dplyr::mutate(labels = paste0(CONCEPT_RELATIONSHIP, " ", FIELD_AS_CONCEPT_02)) %>%
                                dplyr::select(labels) %>%
                                unlist()
                names(x) <- labels
                
                x <- x %>%
                        dplyr::mutate(CONCEPT = paste0(KEY_FIELD, ": ", KEY_CONCEPT_NAME)) %>%
                        dplyr::select(CONCEPT, everything())
                
                as.list(x)
                
        }