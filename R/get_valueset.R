#' Returns the unique values for a given value field in a list
#' @param value_variable column containing the value
#' @param major_id_variable grouping variable used to split the dataframe
#' @param ... other id_variables to supplement the output (ie the actual labels to the id variable)
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @export
get_valueset <-
        function(dataframe, id_variable, value_variable) {
                value_variable    <- enquo(value_variable)
                id_variable <- enquo(id_variable)
                
                list <- split_dataframe_by_identifier(dataframe, !!id_variable)
                
                output <- list()
                for (i in 1:length(list)) {
                        output[[i]] <- list[[i]] %>%
                                                dplyr::select(!!value_variable) %>%
                                                dplyr::distinct() %>%
                                                unlist()
                }
                names(output) <- names(list)
                return(output)
        }
