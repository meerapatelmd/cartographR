#' View the rownumber per status in progress_list
#' @export

view_progress_list_nrow <-
        function(progress_list) {
                lapply(progress_list, nrow)
        }