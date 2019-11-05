#' Queries MRCONSO UMLS table for the exact phrase and limit 1
#' @import mySeagull
#' @export

query_mrconso_data_exact_limit_one <-
        function(search_word) {
                
                sql_statement <- paste0("SELECT * FROM MRCONSO WHERE STR = '", search_word, "' 
                                                                AND LAT = 'ENG' AND ISPREF = 'Y' LIMIT 1;")
                cohort <- mySeagull::get_query("umls", sql_statement)
                return(cohort)
        }
