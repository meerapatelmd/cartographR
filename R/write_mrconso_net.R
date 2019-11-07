#' Writes a MRCONSO NET file if one does not exist for the given phrase
#' @import dplyr
#' @import readr
#' @import mirroR
#' @import typewriteR
#' @import projektoR
#' @import mySeagull
#' @importFrom crayon bold
#' @export

write_mrconso_net <-
        function(phrase, rm_forward_slash = TRUE) {
                glossary_fn <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv"
                files_list <- list.files("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/NETS", full.names = TRUE)
                glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c"))
                
                phrase_00 <- phrase
                if (rm_forward_slash == TRUE) {
                        phrase_01 <- stringr::str_remove_all(phrase_00, "[/]")
                } else {
                        phrase_01 <- phrase_00
                }
                
                exact_phrase_file_ext <- paste0("[0-9]{12}[_]{1}", phrase_01, "[.]{1}csv$")
                
                
                phrase_02 <- paste0("^", phrase_01, "$")
                
                if (any(grepl(exact_phrase_file_ext, files_list)) == FALSE) {
                                        projektoR::append_csv(glossary_fn,
                                                              dataframe = data.frame(
                                                                      UMLS_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                                                      UMLS_SQL_KEYWORD_ID = as.character(max(as.double(glossary$UMLS_SQL_KEYWORD_ID)) + 1),
                                                                      UMLS_SQL_KEYWORD = phrase_01 #Not `phrase_02` because the regex will not be compatible with SQL QUERIES
                                                              ))
                                        
                                        glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c"))
                                        keyword_id <- glossary %>%
                                                                dplyr::filter(UMLS_SQL_KEYWORD == phrase_01) %>%
                                                                dplyr::select(UMLS_SQL_KEYWORD_ID) %>%
                                                                dplyr::filter(row_number() == 1) %>%
                                                                unlist() %>%
                                                                unname()
                                        
                                        
                                        sql_statement <- paste0("SELECT * FROM MRCONSO WHERE STR LIKE '%", phrase_01, "%';") #Not `phrase_02` because the regex will not be compatible with SQL QUERIES
                                        cohort <- mySeagull::get_query("umls", sql_statement)
                                        
                                        output_fn <- mirroR::create_path_to_file(path_folder = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/NETS",
                                                                          basename = paste0(keyword_id, "_", phrase_01), #Not `phrase_02` because the regex will not be compatible with output filenames
                                                                          file_extension = "csv")
                                        
                                        if (!(file.exists(output_fn))) {
                                                cat("\n")
                                                typewriteR::tell_me(crayon::bold("\t\t", "Full File Path:", output_fn))
                                                typewriteR::tell_me("\t", Sys.time(), "Starting to write csv.")
                                                readr::write_csv(cohort, path = output_fn)
                                                typewriteR::tell_me("\t", Sys.time(), "Successfully wrote csv.")
                                                cat("\n\n\n")
                                        }
                }
        }

