#'Add a new query term from the output of collect_query_terms that is split into a list
#' @param list list object from the collect_query_terms function
#' @param list_index_number the index number of list that the query is added to
#' @param ... character strings of the additional queries that will be added to the list
#' @return a list object with the query/queries added at the appropriate index
#' @import dplyr
#' @import somersaulteR
#' @export
#' 
add_new_query_term <-
        function(list, list_index_number, ...) {
                Args <- list(...)
                dataframe <- list[[list_index_number]]
                
                df_skeleton <- dataframe %>%
                        dplyr::filter(row_number() == 1) %>%
                        dplyr::mutate_at(vars(QUERY_TERM, starts_with("COORDINATE"), starts_with("NET")), 
                                         list(~ . == NA)) %>%
                        somersaulteR::call_mr_clean()
                
                for (i in 1:length(Args)) {
                        add_to_dataframe <-
                                df_skeleton %>%
                                dplyr::mutate(QUERY_TERM = Args[[i]])
                        
                        dataframe <-
                                dplyr::bind_rows(dataframe,
                                                 add_to_dataframe)
                        
                }
                list[[list_index_number]] <- dataframe
                return(list)
        }
