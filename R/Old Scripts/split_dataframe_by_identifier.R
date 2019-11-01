#' Splits a dataframe into a list based on an identifier
#' @param id_variable identifier variable to split by
#' @importFrom dplyr select
#' @export
#' 
split_dataframe_by_identifier <-
        function(dataframe, id_variable) {
                id_variable <- enquo(id_variable)
                split(dataframe, dataframe %>%
                                        dplyr::select(!!id_variable))
        }
