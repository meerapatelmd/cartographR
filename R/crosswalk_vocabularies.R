crosswalk_vocabularies <-
        function(dataframe_01, dataframe_01_xcol_name,
                 crosswalk_df, crosswalk_df_colname_01, crosswalk_df_colname_02,
                 dataframe_02, dataframe_02_xcol_name) {

                require(tidyverse, quietly = TRUE)

                #dataframe_01_xcol_name  <- enquo(dataframe_01_xcol_name)
                #crosswalk_df_colname_01 <- enquo(crosswalk_df_colname_01)
                #crosswalk_df_colname_02 <- enquo(crosswalk_df_colname_02)
                #dataframe_02_xcol_name  <- enquo(dataframe_02_xcol_name)

                crosswalked_data <-
                dataframe_01 %>%
                        full_join(crosswalk_df, by = c(dataframe_01_xcol_name = crosswalk_df_colname_01)) %>%
                        full_join(dataframe_02, by = c(crosswalk_df_colname_02 = dataframe_02_xcol_name))

                return(crosswalked_data)
        }
