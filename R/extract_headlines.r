##' Extract the headlines of an org file.
##'
##' This function is used to extract the headlines matching the regexp
##' pattern.
##' @param x org object as character vector.
##' @return the headlines of an orgfile as a character matrix.
##' @export extract_headlines
##' @seealso \code{extract_tags}, \code{extract_timestamps}
##' and \code{extract_categories} for extractions of other org elements.
extract_headlines <-
    function(x) {
        x %>%
            str_match("^\\*{1, }\\s.+")
    }
