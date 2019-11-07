#' This function takes a column in a dataframe that contains terms to do an exact string match
#' @param dataframe dataframe that contains a column of search terms
#' @param sql_term_col variable that contains the single string of search term
#' @param output_dataframe_name name of r object output that also serves as the bookmark
#' @import dplyr
#' @import readr
#' @import projektoR
#' @import mySeagull
#' @import mirroR
#' @import typewriteR
#' @import mirCat
#' @export

jatpat_mrconso_coordinate <-
        function(dataframe, sql_term_col, output_dataframe_name, script_step_number) {
                
                ##Getting bookmark if it exists
                if (!(exists(output_dataframe_name, envir = globalenv()))) {
                        output_dataframe <- data.frame()
                        assign(output_dataframe_name, output_dataframe, envir = globalenv())
                        start_index <- 1
                } else {
                        output_dataframe <- get(output_dataframe_name, envir = globalenv())
                        
                        if (nrow(output_dataframe) == 0) {
                                start_index <- 1
                        } else {
                                start_index <- 1 + nrow(output_dataframe)
                        }
                }
                
                ##Preparing arugments
                sql_term_col <- enquo(sql_term_col)
                
                ##Getting glossary
                # glossary_fn <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv"
                # glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c")) 
                
                ##Filtering glossary for coordinates only
                # coordinate_glossary <- glossary %>%
                #                         dplyr::filter(UMLS_SQL_KEYWORD_CLASS == "coordinate")
                
                ##Preparing dataframe
                dataframe <-
                        dataframe %>%
                        dplyr::select(!!sql_term_col) %>%
                        dplyr::distinct()
                
                colnames(dataframe) <- "UMLS_SQL_KEYWORD"
                
                ##Anti-joining dataframe to get the values that aren't in the glossary
                new_terms_df <-
                        dataframe %>%
                        #dplyr::anti_join(coordinate_glossary, by = "UMLS_SQL_KEYWORD") %>%
                        dplyr::mutate(COORDINATE_CUI = "") %>%
                        dplyr::mutate(COORDINATE_STR = "")
                
                if (nrow(new_terms_df) > 0) {
                        for (i in start_index:nrow(new_terms_df)) {
                                if (i == 1) {
                                        total_obs <- nrow(new_terms_df)
                                }
                                
                                ##Updating glossary
                                # typewriteR::tell_me(Sys.time(), "Updating glossary with", i, "of", total_obs)
                                # cat("\n")
                                # projektoR::append_csv(glossary_fn,
                                #                       dataframe = data.frame(
                                #                               UMLS_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                #                               UMLS_SQL_KEYWORD_ID = as.character(max(as.double(glossary$UMLS_SQL_KEYWORD_ID)) + 1),
                                #                               UMLS_SQL_KEYWORD = new_terms_df$UMLS_SQL_KEYWORD[i],
                                #                               UMLS_SQL_KEYWORD_CLASS = "coordinate"
                                #                       ))
                                
                                
                                ##Jatpat
                                cat("\n")
                                typewriteR::tell_me(Sys.time(), "Querying MRCONSO STR for", new_terms_df$UMLS_SQL_KEYWORD[i])
                                cat("\n")
                                mrconso_data <- query_mrconso_data_exact_limit_one(new_terms_df$UMLS_SQL_KEYWORD[i])
                                if (nrow(mrconso_data) == 1) {
                                        new_terms_df$COORDINATE_CUI[i] <- mrconso_data$CUI
                                        new_terms_df$COORDINATE_STR[i] <- mrconso_data$STR
                                } else {
                                        new_terms_df$COORDINATE_CUI[i] <- NA
                                        new_terms_df$COORDINATE_STR[i] <- NA
                                }
                                typewriteR::tell_me(Sys.time(), "Finished querying MRCONSO STR for", new_terms_df$UMLS_SQL_KEYWORD[i])
                                cat("\n")
                                
                                ##Updating output_dataframe
                                output_dataframe <- dplyr::bind_rows(output_dataframe, 
                                                                     new_terms_df %>%
                                                                             dplyr::filter(row_number() == i)
                                )
                                
                                ##Saving RDS
                                cat("\n")
                                rds_fn <- paste0(script_step_number, "_", output_dataframe_name, ".RData")
                                saveRDS(output_dataframe, file = rds_fn)
                                typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                                cat("\n")
                                
                                ##Concurrent to saving RDS, assigning robject to global
                                cat("\n")
                                assign(output_dataframe_name, output_dataframe, envir = globalenv())
                                typewriteR::tell_me(output_dataframe_name, "R object updated...")
                                cat("\n")
                                typewriteR::tell_me(i, "of", total_obs, "completed.")
                                cat("\n\n\n")
                                
                        }
                }
        }