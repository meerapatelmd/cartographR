#' Gets KEY data and cleans up based on parameters
#' @param path_to_key full file path to key csv file
#' @param log TRUE if this event will be logged using the log functions from the mirCat package
#' @import mirCat
#' @import somersaulteR
#' @import dplyr
#' @import readr
#' @importFrom lubridate ymd_hms
#' @export

key_from_csv <-
        function(path_to_key, 
                 log = TRUE) {
                
                KEY_00 <- readr::read_csv(path_to_key, col_types = readr::cols(.default = "c"))
                
                KEY_01 <-
                        KEY_01 %>%
                        dplyr::mutate(KEY_TIMESTAMP = lubridate::ymd_hms(KEY_TIMESTAMP)) %>%
                        dplyr::mutate(KEY_FIELD = str_replace_all(KEY_FIELD, "^PERMISSIBLE_VALUE_LABEL$", "KMI_PERMISSIBLE_VALUE_LABEL")) %>%
                        dplyr::group_by(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                        dplyr::arrange(desc(KEY_TIMESTAMP)) %>%
                        dplyr::filter(row_number() == 1) %>%
                        dplyr::ungroup()
                
                if (log == TRUE) {
                        mirCat::log_this_as(project_log_dir = dirname(path_to_key),
                                            project_log_load_comment = "cartographR package KEY data",
                                            project_log_fn = basename(path_to_key),
                                            project_log_fn_md5sum = tools::md5sum(basename(path_to_key)),
                                            project_log_load_timestamp = mirroR::get_timestamp()
                                            ,
                                            #project_log_gsheet_title = gsheet_name,
                                            #project_log_gsheet_tab_name = gsheet_tab,
                                            #project_log_gsheet_key = mirCat::gs_id_from_name(gsheet_name)
                        )
                }
                
                return(KEY_01)
                
        }