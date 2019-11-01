#' Sets the key location to iterate on when making updates
#' @param path path to either googlesheet in the format googlesheet_name.tabname or full file path on local
#' @param is.gsheet boolean. TRUE if KEY is a Google Sheet.
#' @importFrom mirCat list_add
#' @export

set_key_location <-
        function(path, is.gsheet = TRUE, file_path = !is.gsheet) {
                if (is.gsheet == TRUE) {
                        gsheet_path <- path
                        names(gsheet_path) <- c("gsheet_path")
                        
                        if (!exists("CARTOGRAPH", envir = globalenv())) {
                                x <- list()
                                x[[1]] <- gsheet_path
                                names(x) <- "KEY_LOCATION"
                                
                                assign("CARTOGRAPH", value = x, envir = globalenv())
                        } else {
                                cartograph <- CARTOGRAPH
                                if ("KEY_LOCATION" %in% names(cartograph)) {
                                        cartograph[["KEY_LOCATION"]] <- gsheet_path
                                        assign("CARTOGRAPH", value = cartograph, envir = globalenv())
                                } else {
                                        cartograph <- mirCat::list_add(cartograph, gsheet_path, name = "KEY_LOCATION")
                                        assign("CARTOGRAPH", value = cartograph, envir = globalenv())
                                }
                        }
                } else {
                        names(path) <- "file_path"
                        if (!exists("CARTOGRAPH", envir = globalenv())) {
                                x <- list()
                                x[[1]] <- path
                                names(x) <- "KEY_LOCATION"
                                
                                assign("CARTOGRAPH", value = x, envir = globalenv())
                        } else {
                                cartograph <- CARTOGRAPH
                                if ("KEY_LOCATION" %in% names(cartograph)) {
                                        cartograph[["KEY_LOCATION"]] <- path
                                        assign("CARTOGRAPH", value = cartograph, envir = globalenv())
                                } else {
                                        cartograph <- mirCat::list_add(cartograph, path, name = "KEY_LOCATION")
                                        assign("CARTOGRAPH", value = cartograph, envir = globalenv())
                                }
                        }
                }
        }