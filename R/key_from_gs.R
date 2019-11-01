#' Gets KEY data and cleans up based on parameters
#' @param gsheet_name exact name of the Google Sheet that has the KEY table. If there are more than 1 Google Sheets with this name, an error will be returned.
#' @param gsheet_tab name of the Google Sheet tab that has the KEY table. Requirements: KEY_TIMESTAMP, KEY_FIELD, and KEY_CONCEPT_NAME as the grouping variable. 
#' @import mirCat
#' @import somersaulteR
#' @import dplyr
#' @export

key_from_gs <-
        function(gsheet_name,
                 gsheet_tab, 
                 log = TRUE) {
                
                KEY_00 <- mirCat::gs_read_tmp_csv(gsheet_name, tab = gsheet_tab) %>%
                                somersaulteR::call_mr_clean()
                
                KEY_01 <-
                        KEY_01 %>%
                        dplyr::mutate(KEY_TIMESTAMP = lubridate::ymd_hms(KEY_TIMESTAMP)) %>%
                        dplyr::mutate(KEY_FIELD = str_replace_all(KEY_FIELD, "^PERMISSIBLE_VALUE_LABEL$", "KMI_PERMISSIBLE_VALUE_LABEL")) %>%
                        dplyr::group_by(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                        dplyr::arrange(desc(KEY_TIMESTAMP)) %>%
                        dplyr::filter(row_number() == 1) %>%
                        dplyr::ungroup()
                
                if (log == TRUE) {
                        mirCat::log_this_as(project_log_dir = "GoogleSheets",
                                            project_log_load_comment = "cartographR package KEY data",
                                            #project_log_fn = basename(path_to_identity),
                                            #project_log_fn_md5sum = tools::md5sum(basename(path_to_identity)),
                                            project_log_load_timestamp = mirroR::get_timestamp(),
                                            project_log_gsheet_title = gsheet_name,
                                            project_log_gsheet_tab_name = gsheet_tab,
                                            project_log_gsheet_key = mirCat::gs_id_from_name(gsheet_name)
                        )
                }
                
                return(KEY_01)
                
        }