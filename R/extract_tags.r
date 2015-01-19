##' Extract the tags of an org file.
##'
##' This function is used to extract the tags matching the regexp
##' pattern.
##' @param orgfile org object as character vector.
##' @return the tags of an orgfile as a character matrix.
##' @export extract_tags
##' @seealso \code{extract_headlines}, \code{extract_timestamps}
##' and \code{extract_categories} for extractions of other org elements.
extract_tags <-
    function(orgfile) {
        orgfile %>%
            str_match("^\\*{1, }\\s.+") %>%
            str_extract("(:[[:alpha:]]{1, }){1, }:$") %>%
            na.omit() %>%
            word()
    }
