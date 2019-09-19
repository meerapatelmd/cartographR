add_primary_key <-
        function(dataframe, tablename = NULL) {
                if (is.null(tablename)) {
                        pk_col_name <- "ID"
                } else {
                        pk_col_name <- paste0(tablename, "_ID")
                }

                pk_col_name <- enquo(pk_col_name)

                dataframe %>%
                        mutate(!!pk_col_name := row_number()) %>%
                        select(!!pk_col_name, everything())
        }
