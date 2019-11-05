#' Creates a new table of contents from arguments in format: {term}:{definition}
#' @param basename unique basename that will be the output csv filename in the format CATALOGUE/TABLE_OF_CONTENTS_{basename}.csv as well as the column name
#' @import somersaulteR
#' @import dplyr
#' @export
#' 

create_new_table_of_contents <-
        function(basename, ...) {
                Args <- unlist(list(...))
                Args2 <- strsplit(Args, split = ":")
                
                term_var <- paste0(basename, "_", "TERM")
                def_var <- paste0(basename, "_", "DEFINITION")
                
                dataframe <-
                        data.frame(t(sapply(Args2, `[`))) %>%
                        somersaulteR::call_mr_clean() 
                
                colnames(dataframe) <- c(term_var, def_var)
                
                dataframe <-
                        dataframe %>%
                        somersaulteR::add_timestamp_column("TABLE_OF_CONTENTS_TIMESTAMP")
                
                return(dataframe)
        }




DENOVO_CUI_REPORT_TOC <-
        data.frame(DENOVO_BUCKET = c("RADIOLOGY", "PRECOORDINATED", "TRUE_DENOVO"),
                   DENOVO_BUCKET_DEFINITION = c("concepts that are related to a Radiology lexicon that does not have coverage in UMLS Metathesaurus",
                                                "concepts that alone do not map to a single UMLS concept, but can have 100% coverage if a combination of existing concepts are used",
                                                "concepts that cannot achieve 100% coverage using only one or a combination of UMLS Metathesaurus concepts and will require local concept ids")) %>%
        somersaulteR::add_timestamp_column("TABLE_OF_CONTENTS_TIMESTAMP") %>%
        somersaulteR::call_mr_clean()
