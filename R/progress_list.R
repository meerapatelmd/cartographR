#' Fresh upload of IDENTITY and KEY to create a new PROGRESS_LIST object
#' @export

progress_list <-
        function(path_to_identity_csv,
                 path_to_key_csv,
                 filter_out_blank_concepts = TRUE) {
                IDENTITY <- 
                        identity_from_csv(path_to_identity_csv,
                                                       target_cols = c("IDENTITY_ID", "VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL", "FIELD_LABEL"))
                
                KEY <- key_from_csv(path_to_key_csv)
                
                PROGRESS_LIST <- join_progress_list(IDENTITY, KEY, filter_out_blank_concepts = filter_out_blank_concepts)
                return(PROGRESS_LIST)
        }
