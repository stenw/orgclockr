##' Extract the tags of an org file.
##'
##' This function is used to extract the tags matching the regexp
##' pattern.
##' @param x org object as character vector.
##' @return the tags of an orgfile as a character matrix.
##' @export extract_tags
##' @seealso \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_categories} and \code{extract_levels} for
##' extractions of other org elements.
extract_tags <-
    function(x) {
        x %>%
            stringr::str_match("^\\*{1, }\\s.+") %>%
            stringr::str_extract("(:[[:alpha:]]{1, }){1, }:$") %>%
            na.omit() %>%
            stringr::word()
    }
