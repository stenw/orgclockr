##' Extract levels of headings in an org file.
##'
##' Get the \code{org-level} in an org file by counting the prepending
##' heading star characters.
##' @param x org object as character vector.
##' @return the levels of headlines in an orgfile as an integer
##' vector.
##' @export extract_levels
##' @seealso \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_tags} and \code{extract_categories} for extractions
##' of other org elements.
extract_levels <-
    function(x) {
        x %>%
            extract_raw_headlines() %>%
            stringr::str_extract("^\\*{1, }\\ ") %>%
            na.omit() %>%
            stringr::str_length()
    }
