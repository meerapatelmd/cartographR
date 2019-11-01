
update_key_with_new_source_values <-
        function(source_dataframe, key_dataframe, col_name) {
                id_col_name <- paste0(deparse(substitute(col_name)), "_ID")
                timestamp_col_name <- paste0(deparse(substitute(col_name)), "_TIMESTAMP")
                
                col_name <- enquo(col_name)
                timestamp_col_name <- enquo(timestamp_col_name)
                id_col_name <- enquo(id_col_name)
                
                source_values <-
                        source_dataframe %>%
                        dplyr::select(!!col_name) %>%
                        distinct() %>%
                        dplyr::filter(!(is.na(!!col_name))) %>%
                        unlist() %>%
                        as.character()
                
                permissible_values <- 
                        key_dataframe %>%
                        select(!!col_name) %>%
                        distinct() %>%
                        unlist() %>%
                        as.character()
                
                x <- bind_rows(
                        key_dataframe %>%
                                somersaulteR::call_mr_clean(),
                        data.frame(X1 = source_values[!(source_values %in% permissible_values)]) %>%
                                dplyr::mutate(X2 = NA) %>%
                                dplyr::mutate(X3 = mirroR::get_timestamp()) %>%
                                dplyr::rename(!!col_name := X1) %>%
                                dplyr::rename(!!timestamp_col_name := X3) %>%
                                dplyr::rename(!!id_col_name := X2) %>%
                                somersaulteR::call_mr_clean()
                ) %>% dplyr::mutate_at(vars(!!id_col_name), list(~caterpillaR::carry_forward_and_add_one(.)))

                #x <- data.frame(!!col_name == source_values[!(source_values %in% permissible_values)])
                return(x)
                
        }