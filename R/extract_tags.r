##' Extract the tags of an org file.
##'
##' This function is used to extract the tags matching the regexp
##' pattern.
##' @param x org object as character vector.
##' @return the tags of an orgfile as a character vector.
##' @export extract_tags
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_tags()
##' ## [1] ":TagOne:"                 ":TagTwo:"
##' ## [3] ":TagThree:"               ":TagTwo:"
##' ## [5] ":TagTwo:"                 ":TagOne:TagThree:TagTwo:"
##' @seealso \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_categories} and
##' \code{extract_levels} to extract other org elements.
extract_tags <-
    function(x) {
        x %>%
            extract_raw_headlines() %>%
            stringr::str_extract("(:[[:alpha:]]{1, }){1, }:$") %>%
            stringr::word()
    }
