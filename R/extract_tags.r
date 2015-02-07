##' Extract the tags of an org file.
##'
##' This function is used to extract the tags matching the regexp
##' pattern.
##'
##' @details
##' If there is more than one tag for a headline, they will
##' be bundled in one character object separated by one space
##' character. The headlines without any tag will give a \code{NA}
##' value, effectively resulting in a vector of as many elements as
##' the number of headlines in the org file.
##' @param x org object as character vector.
##' @return the tags of an orgfile as a character vector.
##' @export extract_tags
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_tags()
##' ##  [1] "TagOne"                 "TagTwo"                 NA
##' ##  [4] NA                       "TagThree"               "TagTwo"
##' ##  [7] "TagTwo"                 "TagOne TagThree TagTwo" NA
##' ## [10] NA                       NA                       NA
##' @seealso \code{extract_efforts}, \code{extract_todostates},
##' \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_categories} and \code{extract_levels} to extract
##' other org elements.
extract_tags <-
    function(x) {
        x %>%
            extract_raw_headlines() %>%
            na.omit() %>%
            stringr::str_extract("(:[[:alpha:]]{1, }){1, }:$") %>%
            stringr::word() %>%
            stringr::str_replace_all(":", " ") %>%
            stringr::str_trim()
    }
