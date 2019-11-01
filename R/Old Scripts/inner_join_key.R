#' Inner join data with a key on a single join column and a single column in the key that will sub in the dataframe
#' @param dataframe dataframe to map to key
#' @param key dataframe with variables that needed to be joined into the dataframe
#' @param join_col column to join on. Must be present in botrh the dataframe and key
#' @param key_sub_col column to sub. Can be absent in the dataframe, but must be present in the key
#' @import dplyr
#' @export


inner_join_key <-
        function(dataframe, key, join_col, key_sub_col) {
                join_col_as_char <- deparse(substitute(join_col))
                join_col <- enquo(join_col)
                key_sub_col <- enquo(key_sub_col)
                
                dplyr::inner_join(dataframe %>%
                                         dplyr::select(-(!!key_sub_col)), key, by = join_col_as_char)
                        
        }
